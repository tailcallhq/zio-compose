package compose.graphql

import zio.schema.StandardType

/**
 * GraphQL is the AST used to represent a GraphQL query,
 * mutation, subscription or schema. The AST can be
 * generated in two ways viz. from a string. and from a list
 * of connections.
 */
sealed trait Ast {
  def encode: String
}

object Ast {
  final case class Document(definitions: Seq[Definitions]) extends Ast {
    override def encode: String =
      s"\n${definitions.sortBy { case ObjectType(name, _) => name }.map(_.encode).mkString("\n")}\n"
  }

  sealed trait Definitions extends Ast

  final case class ObjectType(name: String, fields: List[Field]) extends Definitions {
    override def encode: String =
      s"type $name {\n${fields.map(s => "  " + s.encode).mkString("\n")}\n}"
  }

  final case class Field(name: String, arguments: List[InputValue], fieldType: FieldType)
      extends Ast {
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

  sealed trait FieldType extends Ast {
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

  final case class InputValue(name: String, fieldType: FieldType) extends Ast {
    override def encode: String = {
      val tpe = fieldType.encode
      s"$name: $tpe"
    }
  }
}
