package compose.graphql

import zio.schema.TypeId.{Nominal, Structural}
import zio.schema.{Schema, StandardType, TypeId}

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait GraphQL {
  self =>

  def encode: String
}

object GraphQL {

  case class Document(definitions: Seq[Definitions]) extends GraphQL {
    override def encode: String = s"\n${definitions.map(_.encode).mkString("\n")}\n"
  }

  sealed trait Definitions extends GraphQL

  case class ObjectType(name: String, fields: List[Field]) extends Definitions {
    override def encode: String =
      s"type $name {\n${fields.map(s => "  " + s.encode).mkString("\n")}\n}"
  }

  case class Field(
    name: String,
    arguments: List[InputValue],
    fieldType: NamedType,
    isList: Boolean,
    isOptional: Boolean,
  ) extends GraphQL {
    override def encode: String = {
      val args = if (arguments.isEmpty) "" else s"(${arguments.map(_.encode).mkString(", ")})"
      val tpe  = if (isList) s"[${fieldType.encode}]" else fieldType.encode
      val opt  = if (isOptional) "" else "!"
      s"$name$args: $tpe$opt"
    }
  }

  case class NamedType(name: String) extends GraphQL {
    override def encode: String = name
  }

  case class InputValue(name: String, fieldType: NamedType, isList: Boolean, isOptional: Boolean)
      extends GraphQL {
    override def encode: String = {
      val tpe = if (isList) s"[${fieldType.encode}]" else fieldType.encode
      val opt = if (isOptional) "" else "!"
      s"$name: $tpe$opt"
    }
  }

  def isOption(schema: Schema[_]): Boolean = schema match {
    case Schema.Optional(_, _) => true
    case _                     => false
  }

  def isList(schema: Schema[_]): Boolean = schema match {
    case Schema.Sequence(_, _, _, _, _) => true
    case _                              => false
  }

  def isRecord(schema: Schema[_]): Boolean = schema match {
    case _: Schema.Record[_] => true
    case _                   => false
  }

  def getArguments(schema: Schema[_]): List[InputValue] = schema match {
    case schema: Schema.Record[_] => schema.structure.map { field =>
        InputValue(
          field.label,
          getNamedType(field.schema),
          isList(field.schema),
          isOption(field.schema),
        )
      }.toList

    case schema @ Schema.Primitive(_, _) =>
      InputValue("value", getNamedType(schema), isList(schema), isOption(schema)) :: Nil
    case _                               => Nil
  }

  def getFields(schema: Schema.Record[_]): List[Field] = schema.structure.map(field =>
    Field(
      field.label,
      Nil,
      getNamedType(field.schema),
      isList(field.schema),
      isOption(field.schema),
    ),
  ).toList

  def getObjectType(schema: Schema[_]): Seq[ObjectType] = {
    schema match {
      case Schema.Optional(schema, _) => getObjectType(schema)

      case Schema.Sequence(schemaA, _, _, _, _) => getObjectType(schemaA)

      case schema: Schema.Record[_] =>
        val children = schema.structure.map(_.schema).filter(isRecord).flatMap(getObjectType)
        children ++ Seq(ObjectType(getName(schema.id), getFields(schema)))

      case Schema.Primitive(_, _) => Seq(ObjectType("Query", Nil))

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

  @tailrec
  def getNamedType(schema: Schema[_]): NamedType = {
    schema match {
      case Schema.Optional(schema, _) => getNamedType(schema)

      case Schema.Sequence(schemaA, _, _, _, _) => getNamedType(schemaA)

      case schema: Schema.Record[_] => NamedType(schema.id.name)

      case Schema.Lazy(schema0) => getNamedType(schema0())

      case Schema.Primitive(standardType, _) =>
        import StandardType._
        standardType match {
          case CharType              => NamedType("Char")
          case MonthDayType          => NamedType("MonthDay")
          case OffsetDateTimeType(_) => NamedType("OffsetDateTime")
          case StringType            => NamedType("String")
          case DurationType          => NamedType("Duration")
          case UUIDType              => NamedType("UUID")
          case ByteType              => NamedType("Byte")
          case DayOfWeekType         => NamedType("DayOfWeek")
          case InstantType(_)        => NamedType("Instant")
          case ZonedDateTimeType(_)  => NamedType("ZonedDateTime")
          case YearType              => NamedType("Year")
          case MonthType             => NamedType("Month")
          case BigDecimalType        => NamedType("BigDecimal")
          case UnitType              => NamedType("Unit")
          case BinaryType            => NamedType("Binary")
          case BoolType              => NamedType("Bool")
          case IntType               => NamedType("Int")
          case LocalDateType(_)      => NamedType("LocalDate")
          case DoubleType            => NamedType("Double")
          case LongType              => NamedType("Long")
          case LocalDateTimeType(_)  => NamedType("LocalDateTime")
          case ZoneIdType            => NamedType("ZoneId")
          case FloatType             => NamedType("Float")
          case BigIntegerType        => NamedType("BigInteger")
          case YearMonthType         => NamedType("YearMonth")
          case OffsetTimeType(_)     => NamedType("OffsetTime")
          case PeriodType            => NamedType("Period")
          case ZoneOffsetType        => NamedType("ZoneOffset")
          case ShortType             => NamedType("Short")
          case LocalTimeType(_)      => NamedType("LocalTime")
        }

      // Unhandled
//      case Schema.Tuple(left, right, annotations)                => ???
//      case Schema.Fail(message, annotations)                     => ???
//      case Schema.MapSchema(ks, vs, annotations)                 => ???
//      case Schema.SetSchema(as, annotations)                     => ???
//      case schema: Schema.Enum[_]                                => ???
//      case Schema.Dynamic(annotations)                           => ???
//      case Schema.Transform(schema, f, g, annotations, identity) => ???
//      case Schema.Meta(ast, annotations)                         => ???
//      case Schema.SemiDynamic(defaultValue, annotations)         => ???
//      case Schema.EitherSchema(left, right, annotations)         => ???
      case schema                            => throw new MatchError(schema)
    }
  }

  def getTypeDefinitions(connections: Seq[Connection]): Seq[GraphQL.ObjectType] = {
    val definitions = mutable.Set.empty[GraphQL.ObjectType]

    connections.foreach { case Connection.Cons(name, arg, from, to, _) =>
      val fromName      = getObjectType(from)
      val toName        = getObjectType(to)
      val conField      = Field(name, getArguments(arg), getNamedType(to), isList(to), isOption(to))
      val conObjectType = ObjectType(getName(from), conField :: Nil)
      definitions.addAll(fromName).addAll(toName).add(conObjectType)
    }

    mergeTypeDefinitions(definitions.toSeq)
  }

  def mergeTypeDefinitions(typeDefinitions: Seq[ObjectType]): Seq[ObjectType] = {
    val merged = mutable.Map.empty[String, ObjectType]

    typeDefinitions.foreach { definition =>
      merged.get(definition.name) match {
        case Some(ObjectType(name, fields)) => merged
            .put(name, ObjectType(name, fields ++ definition.fields))
        case None                           => merged.put(definition.name, definition)
      }
    }

    merged.values.toSeq
  }

  def from(connections: Seq[Connection]): GraphQL = {
    GraphQL.Document(getTypeDefinitions(connections))
  }
}
