package compose.graphql

import zio.schema.TypeId.{Nominal, Structural}
import zio.schema.{Schema, StandardType, TypeId}

import scala.collection.mutable

/**
 * GraphQL is the AST used to represent a GraphQL query,
 * mutation, subscription or schema. The AST can be
 * generated in two ways viz. from a string. and from a list
 * of connections.
 */
sealed trait GraphQL {
  def encode: String
}

object GraphQL {

  final case class Document(definitions: Seq[Definitions]) extends GraphQL {
    override def encode: String =
      s"\n${definitions.sortBy { case ObjectType(name, _) => name }.map(_.encode).mkString("\n")}\n"
  }

  sealed trait Definitions extends GraphQL

  final case class ObjectType(name: String, fields: List[Field]) extends Definitions {
    override def encode: String =
      s"type $name {\n${fields.map(s => "  " + s.encode).mkString("\n")}\n}"
  }

  final case class Field(name: String, arguments: List[InputValue], fieldType: FieldType)
      extends GraphQL {
    self =>
    override def encode: String = {
      val args = if (arguments.isEmpty) "" else s"(${arguments.map(_.encode).mkString(", ")})"
      val tpe  = fieldType.encode
      s"$name$args: $tpe"
    }

    def asRequired: Field                    = self.copy(fieldType = fieldType.asRequired)
    def asList: Field                        = self.copy(fieldType = fieldType.asList)
    def asRequiredWhen(cond: Boolean): Field = if (cond) asRequired else self
    def asListWhen(cond: Boolean): Field     = if (cond) asList else self
  }

  sealed trait FieldType extends GraphQL {
    self =>
    final def asRequired: FieldType                    = FieldType.RequiredFieldType(self)
    final def asList: FieldType                        = FieldType.ListFieldType(self)
    final def asRequiredWhen(cond: Boolean): FieldType = if (cond) asRequired else self
    final def asListWhen(cond: Boolean): FieldType     = if (cond) asList else self
  }
  object FieldType {
    final case class NamedFieldType(name: String) extends FieldType {
      override def encode: String = name
    }

    final case class RequiredFieldType(tpe: FieldType) extends FieldType {
      override def encode: String = s"${tpe.encode}!"
    }

    final case class ListFieldType(tpe: FieldType) extends FieldType {
      override def encode: String = s"[${tpe.encode}]"
    }

    def from(standardType: StandardType[_]): NamedFieldType = {
      import StandardType._
      standardType match {
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
    }
  }

  final case class InputValue(name: String, fieldType: FieldType) extends GraphQL {
    override def encode: String = {
      val tpe = fieldType.encode
      s"$name: $tpe"
    }
  }

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

  def getObjectType(schema: Schema[_]): Seq[ObjectType] = {
    reduceSchema(schema) match {
      case Schema.Optional(schema, _) => getObjectType(schema)

      case Schema.Sequence(schemaA, _, _, _, _) => getObjectType(schemaA)

      case schema: Schema.Record[_] =>
        val children = schema.structure.map(_.schema).filter(isRecord).flatMap(getObjectType)
        val fields   = Seq(ObjectType(getName(schema.id), getFields(schema)))
        children ++ fields

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

  def getFieldType(schema: Schema[_]): FieldType = {
    def loop(schema: Schema[_], isRequired: Boolean): FieldType = {
      reduceSchema(schema) match {
        case Schema.Optional(schema, _)           => loop(schema, false)
        case Schema.Sequence(schemaA, _, _, _, _) => getFieldType(schemaA).asList
            .asRequiredWhen(isRequired)
        case schema: Schema.Record[_]             => FieldType.NamedFieldType(getName(schema.id))
            .asRequiredWhen(isRequired)
        case Schema.Primitive(standardType, _)    => FieldType.from(standardType)
            .asRequiredWhen(isRequired)

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
        case schema                               => throw new MatchError(schema)
      }
    }

    loop(schema, true)

  }

  def getTypeDefinitions(connections: Seq[Connection]): Seq[GraphQL.ObjectType] = {
    val definitions = mutable.Set.empty[GraphQL.ObjectType]

    connections.foreach { case Connection.Cons(name, arg, from, to, _) =>
      val fromName      = getObjectType(from)
      val toName        = getObjectType(to)
      val conField      = Field(name, getArguments(arg), getFieldType(to))
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

  def from(connection: Connection): GraphQL = { from(Seq(connection)) }
}
