package compose.graphql.ast

import compose.graphql.GraphQL
import compose.graphql.SchemaExtensions.Extensions
import compose.graphql.ast.Ast.Type.Named
import compose.graphql.ast.Ast.{Definitions, Type}
import zio.schema.TypeId.{Nominal, Structural}
import zio.schema.{Schema, StandardType, TypeId}

import scala.collection.mutable

case object AstGenerator {
  def getArguments(schema: Schema[_]): List[Definitions.InputValue] = schema match {
    case schema: Schema.Record[_] => schema.structure.map { field =>
        Definitions.InputValue(field.label, getFieldType(field.schema))
      }.toList

    case schema @ Schema.Primitive(standardType, _) if standardType != StandardType.UnitType =>
      Definitions.InputValue("value", getFieldType(schema)) :: Nil

    case _ => Nil
  }

  def getFields(schema: Schema.Record[_]): List[Definitions.Field] = schema.structure
    .map(field => Definitions.Field(field.label, Nil, getFieldType(field.schema))).toList

  def getObjectType(schema: Schema[_]): Seq[Definitions.ObjectType] = {
    schema.eval match {
      case Schema.Optional(schema, _) => getObjectType(schema)

      case Schema.Sequence(schemaA, _, _, _, _) => getObjectType(schemaA)

      case schema: Schema.Record[_] =>
        val children = schema.structure.map(_.schema).filter(_.isRecord).flatMap(getObjectType)
        val fields   = Seq(Definitions.ObjectType(getName(schema.id), getFields(schema)))
        children ++ fields

      case Schema.Primitive(_, _) => Seq(Definitions.ObjectType("Query", Nil))

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
          val fieldType = Type.List(getFieldType(schemaA))
          if (isRequired) Type.NotNull(fieldType) else fieldType

        case schema: Schema.Record[_] =>
          val fieldType = Type.Named(getName(schema.id))
          if (isRequired) Type.NotNull(fieldType) else fieldType

        case Schema.Primitive(standardType, _) =>
          import StandardType._
          val fieldType = standardType match {
            case BigDecimalType        => Named("BigDecimal")
            case BigIntegerType        => Named("BigInteger")
            case BinaryType            => Named("Binary")
            case BoolType              => Named("Boolean")
            case ByteType              => Named("Byte")
            case CharType              => Named("Char")
            case DayOfWeekType         => Named("DayOfWeek")
            case DoubleType            => Named("Float")
            case DurationType          => Named("Duration")
            case FloatType             => Named("Float")
            case InstantType(_)        => Named("Instant")
            case IntType               => Named("Int")
            case LocalDateTimeType(_)  => Named("LocalDateTime")
            case LocalDateType(_)      => Named("LocalDate")
            case LocalTimeType(_)      => Named("LocalTime")
            case LongType              => Named("Long")
            case MonthDayType          => Named("MonthDay")
            case MonthType             => Named("Month")
            case OffsetDateTimeType(_) => Named("OffsetDateTime")
            case OffsetTimeType(_)     => Named("OffsetTime")
            case PeriodType            => Named("Period")
            case ShortType             => Named("Short")
            case StringType            => Named("String")
            case UnitType              => Named("Unit")
            case UUIDType              => Named("ID")
            case YearMonthType         => Named("YearMonth")
            case YearType              => Named("Year")
            case ZonedDateTimeType(_)  => Named("ZonedDateTime")
            case ZoneIdType            => Named("ZoneId")
            case ZoneOffsetType        => Named("ZoneOffset")
          }

          if (isRequired) Type.NotNull(fieldType) else fieldType

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

  def getTypeDefinitions(connections: Seq[GraphQL]): Seq[Definitions.ObjectType] = {
    val definitions = mutable.Set.empty[Definitions.ObjectType]

    connections.foreach { case GraphQL.Cons(name, arg, from, to, _) =>
      val fromName      = getObjectType(from)
      val toName        = getObjectType(to)
      val conField      = Definitions.Field(name, getArguments(arg), getFieldType(to))
      val conObjectType = Definitions.ObjectType(getName(from), conField :: Nil)
      definitions.addAll(fromName).addAll(toName).add(conObjectType)
    }

    mergeTypeDefinitions(definitions.toSeq)
  }

  def mergeTypeDefinitions(
    typeDefinitions: Seq[Definitions.ObjectType],
  ): Seq[Definitions.ObjectType] = {
    val merged = mutable.Map.empty[String, Definitions.ObjectType]

    typeDefinitions.foreach { definition =>
      merged.get(definition.name) match {
        case Some(Definitions.ObjectType(name, fields)) => merged
            .put(name, Definitions.ObjectType(name, fields ++ definition.fields))
        case None                                       => merged.put(definition.name, definition)
      }
    }

    merged.values.toSeq
  }

  def gen(connections: Seq[GraphQL]): Ast = { Ast.Document(getTypeDefinitions(connections)) }
}
