package compose.graphql.internal

import zio.Chunk
import zio.json.ast.Json
import zio.schema.{DynamicValue, StandardType}

private[compose] object JsonUtility {
  def fromStandardType[A](value: A, standardType: StandardType[A]): Json = {
    standardType match {
      case StandardType.UnitType           => Json.Obj()
      case StandardType.StringType         => Json.Str(value.toString)
      case StandardType.BoolType           => Json.Bool(value.toString.toBoolean)
      case StandardType.IntType            => Json.Num(value.toString.toInt)
      case StandardType.ByteType           => Json.Num(value.asInstanceOf[Byte].toDouble)
      case StandardType.ShortType          => ???
      case StandardType.LongType           => ???
      case StandardType.FloatType          => ???
      case StandardType.DoubleType         => ???
      case StandardType.BinaryType         => ???
      case StandardType.CharType           => ???
      case StandardType.UUIDType           => ???
      case StandardType.BigDecimalType     => ???
      case StandardType.BigIntegerType     => ???
      case StandardType.DayOfWeekType      => ???
      case StandardType.MonthType          => ???
      case StandardType.MonthDayType       => ???
      case StandardType.PeriodType         => ???
      case StandardType.YearType           => ???
      case StandardType.YearMonthType      => ???
      case StandardType.ZoneIdType         => ???
      case StandardType.ZoneOffsetType     => ???
      case StandardType.DurationType       => ???
      case StandardType.InstantType        => ???
      case StandardType.LocalDateType      => ???
      case StandardType.LocalTimeType      => ???
      case StandardType.LocalDateTimeType  => ???
      case StandardType.OffsetTimeType     => ???
      case StandardType.OffsetDateTimeType => ???
      case StandardType.ZonedDateTimeType  => ???
    }
  }

  def fromDynamicValue(value: DynamicValue): Json = value match {
    case DynamicValue.Record(_, values)              => Json
        .Obj(Chunk.fromIterable(values).map { case (key, value) => key -> fromDynamicValue(value) })
    case DynamicValue.Enumeration(id, value)         => Json
        .Obj(id.name -> Json.Obj(value._1 -> fromDynamicValue(value._2)))
    case DynamicValue.Sequence(values)               => Json.Arr(values.map(fromDynamicValue))
    case DynamicValue.Dictionary(entries)            => Json.Arr(entries.map { case (key, value) =>
        Json.Obj("key" -> fromDynamicValue(key), "value" -> fromDynamicValue(value))
      })
    case DynamicValue.SetValue(values)               => Json
        .Arr(Chunk.fromIterable(values.map(value => fromDynamicValue(value))))
    case DynamicValue.Primitive(value, standardType) => fromStandardType(value, standardType)
    case DynamicValue.Singleton(_)                   => Json.Obj()
    case DynamicValue.SomeValue(value)               => fromDynamicValue(value)
    case DynamicValue.NoneValue                      => Json.Null
    case DynamicValue.Tuple(left, right)             => Json
        .Arr(fromDynamicValue(left), fromDynamicValue(right))
    case DynamicValue.LeftValue(value)               => Json.Obj("error" -> fromDynamicValue(value))
    case DynamicValue.RightValue(value)              => fromDynamicValue(value)
    case DynamicValue.DynamicAst(_)                  => Json.Obj()
    case DynamicValue.Error(message)                 => Json.Obj("error" -> Json.Str(message))
  }
}
