package compose.graphql

import compose.Interpreter
import zio.schema.StandardType
import caliban.Value
import caliban.schema.Step
import zio.schema.DynamicValue
import zio.query.ZQuery
import zio.schema.meta.MetaSchema

final class StepGenerator(graph: Graph, i: Interpreter) {

  def toValue(input: StandardType[_], result: Any): Value = input match {
    case StandardType.IntType    => Value.IntValue(result.asInstanceOf[Int])
    case StandardType.BoolType   => Value.BooleanValue(result.asInstanceOf[Boolean])
    case StandardType.StringType => Value.StringValue(result.asInstanceOf[String])

    // TODO: implement the rest of the cases
    case StandardType.ByteType           => ???
    case StandardType.CharType           => ???
    case StandardType.LongType           => ???
    case StandardType.BinaryType         => ???
    case StandardType.DayOfWeekType      => ???
    case StandardType.ZonedDateTimeType  => ???
    case StandardType.OffsetTimeType     => ???
    case StandardType.ShortType          => ???
    case StandardType.LocalTimeType      => ???
    case StandardType.YearMonthType      => ???
    case StandardType.LocalDateType      => ???
    case StandardType.UUIDType           => ???
    case StandardType.ZoneOffsetType     => ???
    case StandardType.MonthDayType       => ???
    case StandardType.YearType           => ???
    case StandardType.OffsetDateTimeType => ???
    case StandardType.BigDecimalType     => ???
    case StandardType.FloatType          => ???
    case StandardType.PeriodType         => ???
    case StandardType.BigIntegerType     => ???
    case StandardType.UnitType           => ???
    case StandardType.MonthType          => ???
    case StandardType.DurationType       => ???
    case StandardType.ZoneIdType         => ???
    case StandardType.InstantType        => ???
    case StandardType.LocalDateTimeType  => ???
    case StandardType.DoubleType         => ???
  }

  def resolve(meta: MetaSchema, result: DynamicValue): Step[Any] = {
    meta match {
      case MetaSchema.Product(id, _, fields, _) => Step.ObjectStep(
          id.name,
          fields.map { case (label, meta) =>
            label -> {
              result match {
                case DynamicValue.Record(_, values) => resolve(meta, values(label))
                case result                         => throw new MatchError(result)
              }
            }
          }.toMap,
        )

      case MetaSchema.ListNode(meta, _, _) => result match {
          case DynamicValue.Sequence(chunk) => Step
              .ListStep(chunk.map(result => resolve(meta, result)).toList)
          case result                       => throw new MatchError(result)
        }

      case MetaSchema.Value(valueType, _, _) => result match {
          case DynamicValue.Primitive(result, _) => Step.PureStep(toValue(valueType, result))
          case result                            => throw new MatchError(result)
        }

      // case MetaSchema.Dictionary(keys, values, path, optional) => ???
      // case MetaSchema.Dynamic(withSchema, path, optional)      => ???
      // case MetaSchema.Tuple(path, left, right, optional)       => ???
      // case MetaSchema.Ref(refPath, path, optional)             => ???
      // case MetaSchema.FailNode(message, path, optional)        => ???
      // case MetaSchema.Either(path, left, right, optional)      => ???
      // case MetaSchema.Sum(id, path, cases, optional)           => ???
      case schema => throw new MatchError(schema)
    }

  }

  def resolve(graph: Graph): Step[Any] = {

    Step.ObjectStep(
      "Query",
      graph.cons.map(cons =>
        cons.name -> {
          val result = i.evalDynamic(cons.executable, DynamicValue {})
          val query  = ZQuery.fromZIO(result).map(result => resolve(cons.toType.ast, result))
          Step.QueryStep(query)
        },
      ).toMap,
    )
  }

  def resolve: Step[Any] = resolve(graph)
}
