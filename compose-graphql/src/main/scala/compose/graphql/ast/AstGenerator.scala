package compose.graphql.ast

import compose.graphql.Connection
import compose.graphql.SchemaExtensions.Extensions
import compose.graphql.ast.Ast.Type.NamedType
import compose.graphql.ast.Ast.{Definitions, Type}
import zio.schema.TypeId.{Nominal, Structural}
import zio.schema.{Schema, StandardType, TypeId}

import scala.collection.mutable

case object AstGenerator {
  def getArguments(schema: Schema[_]): List[Definitions.InputValueDefinition] = schema match {
    case schema: Schema.Record[_] => schema.structure.map { field =>
        Definitions.InputValueDefinition(field.label, getFieldType(field.schema))
      }.toList

    case schema @ Schema.Primitive(standardType, _) if standardType != StandardType.UnitType =>
      Definitions.InputValueDefinition("value", getFieldType(schema)) :: Nil

    case _ => Nil
  }

  def getFields(schema: Schema.Record[_]): List[Definitions.FieldDefinition] = schema.structure
    .map(field => Definitions.FieldDefinition(field.label, Nil, getFieldType(field.schema))).toList

  def getObjectType(schema: Schema[_]): Seq[Definitions.ObjectTypeDefinition] = {
    schema.eval match {
      case Schema.Optional(schema, _) => getObjectType(schema)

      case Schema.Sequence(schemaA, _, _, _, _) => getObjectType(schemaA)

      case schema: Schema.Record[_] =>
        val children = schema.structure.map(_.schema).filter(_.isRecord).flatMap(getObjectType)
        val fields   = Seq(Definitions.ObjectTypeDefinition(getName(schema.id), getFields(schema)))
        children ++ fields

      case Schema.Primitive(_, _) => Seq(Definitions.ObjectTypeDefinition("Query", Nil))

      // Unhandled
      //      case Schema.Tuple(left, right, annotations)                => ???
      //      case Schema.Fail(message, annotations)                     => ???
      //      case Schema.MapSchema(ks, vs, annotations)                 => ???
      //      case Schema.SetSchema(as, annotations)                     => ???
      //      case schema: Schema.Enum[_]                                => ???
      //      case Schema.Dynamic(annotations)                           => ???
      //      case Schema.Transform(schema, f, g, annotations, identity) => ???
      //      case Schema.Meta(ast, annotations)                         => ???
      //      case Schema.Lazy(schema0)                                  => ???
      //      case Schema.SemiDynamic(defaultValue, annotations)         => ???
      //      case Schema.EitherSchema(left, right, annotations)         => ???
      case schema => throw new MatchError(schema)
    }
  }

  def getName(typeId: TypeId): String = {
    typeId match {
      case Nominal(_, objectNames, typeName) => objectNames.mkString("") + typeName
      case Structural                        => "Structural"
    }
  }

  def getName(schema: Schema[_]): String = {
    schema match {
      case schema: Schema.Record[_]                   => getName(schema.id)
      case Schema.Primitive(StandardType.UnitType, _) => "Query"
      case _                                          => ???
    }
  }

  def getFieldType(schema: Schema[_]): Type = {
    def loop(schema: Schema[_], isRequired: Boolean): Type = {
      schema.eval match {
        case Schema.Optional(schema, _) => loop(schema, false)

        case Schema.Sequence(schemaA, _, _, _, _) =>
          val fieldType = Type.ListType(getFieldType(schemaA))
          if (isRequired) Type.NotNullType(fieldType) else fieldType

        case schema: Schema.Record[_] =>
          val fieldType = Type.NamedType(getName(schema.id))
          if (isRequired) Type.NotNullType(fieldType) else fieldType

        case Schema.Primitive(standardType, _) =>
          import StandardType._
          val fieldType = standardType match {
            case BigDecimalType        => NamedType("BigDecimal")
            case BigIntegerType        => NamedType("BigInteger")
            case BinaryType            => NamedType("Binary")
            case BoolType              => NamedType("Boolean")
            case ByteType              => NamedType("Byte")
            case CharType              => NamedType("Char")
            case DayOfWeekType         => NamedType("DayOfWeek")
            case DoubleType            => NamedType("Float")
            case DurationType          => NamedType("Duration")
            case FloatType             => NamedType("Float")
            case InstantType(_)        => NamedType("Instant")
            case IntType               => NamedType("Int")
            case LocalDateTimeType(_)  => NamedType("LocalDateTime")
            case LocalDateType(_)      => NamedType("LocalDate")
            case LocalTimeType(_)      => NamedType("LocalTime")
            case LongType              => NamedType("Long")
            case MonthDayType          => NamedType("MonthDay")
            case MonthType             => NamedType("Month")
            case OffsetDateTimeType(_) => NamedType("OffsetDateTime")
            case OffsetTimeType(_)     => NamedType("OffsetTime")
            case PeriodType            => NamedType("Period")
            case ShortType             => NamedType("Short")
            case StringType            => NamedType("String")
            case UnitType              => NamedType("Unit")
            case UUIDType              => NamedType("ID")
            case YearMonthType         => NamedType("YearMonth")
            case YearType              => NamedType("Year")
            case ZonedDateTimeType(_)  => NamedType("ZonedDateTime")
            case ZoneIdType            => NamedType("ZoneId")
            case ZoneOffsetType        => NamedType("ZoneOffset")
          }

          if (isRequired) Type.NotNullType(fieldType) else fieldType

        // Unhandled
        //      case Schema.Tuple(_, _, _)                => ???
        //      case Schema.Fail(message, annotations)                     => ???
        //      case Schema.MapSchema(ks, vs, annotations)                 => ???
        //      case Schema.SetSchema(as, annotations)                     => ???
        //      case schema: Schema.Enum[_]                                => ???
        //      case Schema.Dynamic(annotations)                           => ???
        //      case Schema.Transform(schema, _, _, _, _)                  => ???
        //      case Schema.Meta(ast, annotations)                         => ???
        //      case Schema.SemiDynamic(defaultValue, annotations)         => ???
        //      case Schema.EitherSchema(left, right, annotations)         => ???
        case schema                            => throw new MatchError(schema)
      }
    }

    loop(schema, true)

  }

  def getTypeDefinitions(connections: Seq[Connection]): Seq[Definitions.ObjectTypeDefinition] = {
    val definitions = mutable.Set.empty[Definitions.ObjectTypeDefinition]

    connections.foreach { case Connection.Cons(name, arg, from, to, _) =>
      val fromName      = getObjectType(from)
      val toName        = getObjectType(to)
      val conField      = Definitions.FieldDefinition(name, getArguments(arg), getFieldType(to))
      val conObjectType = Definitions.ObjectTypeDefinition(getName(from), conField :: Nil)
      definitions.addAll(fromName).addAll(toName).add(conObjectType)
    }

    mergeTypeDefinitions(definitions.toSeq)
  }

  def mergeTypeDefinitions(
    typeDefinitions: Seq[Definitions.ObjectTypeDefinition],
  ): Seq[Definitions.ObjectTypeDefinition] = {
    val merged = mutable.Map.empty[String, Definitions.ObjectTypeDefinition]

    typeDefinitions.foreach { definition =>
      merged.get(definition.name) match {
        case Some(Definitions.ObjectTypeDefinition(name, fields)) => merged
            .put(name, Definitions.ObjectTypeDefinition(name, fields ++ definition.fields))
        case None => merged.put(definition.name, definition)
      }
    }

    merged.values.toSeq
  }

  def gen(connections: Seq[Connection]): Ast = { Ast.Document(getTypeDefinitions(connections)) }
}
