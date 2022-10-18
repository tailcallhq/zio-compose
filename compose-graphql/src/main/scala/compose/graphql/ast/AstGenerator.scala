package compose.graphql.ast

import compose.graphql.GraphQL
import compose.graphql.ast.Ast.FieldType.NamedFieldType
import compose.graphql.ast.Ast.{Definitions, Field, FieldType, InputValue}
import zio.schema.TypeId.{Nominal, Structural}
import zio.schema.{Schema, StandardType, TypeId}

import scala.collection.mutable

case object AstGenerator {
  def reduceSchema[A](schema: Schema[A]): Schema[A] = schema match {
    case Schema.Lazy(schema0) => schema0()
    case schema               => schema
  }

  def isOption(schema: Schema[_]): Boolean = {
    reduceSchema(schema) match {
      case _: Schema.Optional[_] => true
      case _                     => false
    }
  }

  def isList(schema: Schema[_]): Boolean = reduceSchema(schema) match {
    case Schema.Sequence(_, _, _, _, _) => true
    case _                              => false
  }

  def isRecord(schema: Schema[_]): Boolean = schema match {
    case _: Schema.Record[_] => true
    case _                   => false
  }

  def getArguments(schema: Schema[_]): List[InputValue] = schema match {
    case schema: Schema.Record[_] => schema.structure.map { field =>
        InputValue(field.label, getFieldType(field.schema))
      }.toList

    case schema @ Schema.Primitive(standardType, _) if standardType != StandardType.UnitType =>
      InputValue("value", getFieldType(schema)) :: Nil
    case _                                                                                   => Nil
  }

  def getFields(schema: Schema.Record[_]): List[Field] = schema.structure.map { field =>
    Field(field.label, Nil, getFieldType(field.schema))
  }.toList

  def getObjectType(schema: Schema[_]): Seq[Definitions.ObjectType] = {
    reduceSchema(schema) match {
      case Schema.Optional(schema, _) => getObjectType(schema)

      case Schema.Sequence(schemaA, _, _, _, _) => getObjectType(schemaA)

      case schema: Schema.Record[_] =>
        val children = schema.structure.map(_.schema).filter(isRecord).flatMap(getObjectType)
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

  def getFieldType(schema: Schema[_]): FieldType = {
    def loop(schema: Schema[_], isRequired: Boolean): FieldType = {
      reduceSchema(schema) match {
        case Schema.Optional(schema, _) => loop(schema, false)

        case Schema.Sequence(schemaA, _, _, _, _) =>
          val fieldType = FieldType.ListFieldType(getFieldType(schemaA))
          if (isRequired) FieldType.RequiredFieldType(fieldType) else fieldType

        case schema: Schema.Record[_] =>
          val fieldType = FieldType.NamedFieldType(getName(schema.id))
          if (isRequired) FieldType.RequiredFieldType(fieldType) else fieldType

        case Schema.Primitive(standardType, _) =>
          import StandardType._
          val fieldType = standardType match {
            case BigDecimalType        => NamedFieldType("BigDecimal")
            case BigIntegerType        => NamedFieldType("BigInteger")
            case BinaryType            => NamedFieldType("Binary")
            case BoolType              => NamedFieldType("Boolean")
            case ByteType              => NamedFieldType("Byte")
            case CharType              => NamedFieldType("Char")
            case DayOfWeekType         => NamedFieldType("DayOfWeek")
            case DoubleType            => NamedFieldType("Float")
            case DurationType          => NamedFieldType("Duration")
            case FloatType             => NamedFieldType("Float")
            case InstantType(_)        => NamedFieldType("Instant")
            case IntType               => NamedFieldType("Int")
            case LocalDateTimeType(_)  => NamedFieldType("LocalDateTime")
            case LocalDateType(_)      => NamedFieldType("LocalDate")
            case LocalTimeType(_)      => NamedFieldType("LocalTime")
            case LongType              => NamedFieldType("Long")
            case MonthDayType          => NamedFieldType("MonthDay")
            case MonthType             => NamedFieldType("Month")
            case OffsetDateTimeType(_) => NamedFieldType("OffsetDateTime")
            case OffsetTimeType(_)     => NamedFieldType("OffsetTime")
            case PeriodType            => NamedFieldType("Period")
            case ShortType             => NamedFieldType("Short")
            case StringType            => NamedFieldType("String")
            case UnitType              => NamedFieldType("Unit")
            case UUIDType              => NamedFieldType("ID")
            case YearMonthType         => NamedFieldType("YearMonth")
            case YearType              => NamedFieldType("Year")
            case ZonedDateTimeType(_)  => NamedFieldType("ZonedDateTime")
            case ZoneIdType            => NamedFieldType("ZoneId")
            case ZoneOffsetType        => NamedFieldType("ZoneOffset")
          }

          if (isRequired) FieldType.RequiredFieldType(fieldType) else fieldType

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
      val conField      = Field(name, getArguments(arg), getFieldType(to))
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

  def gen(connection: GraphQL): Ast = { gen(Seq(connection)) }
}