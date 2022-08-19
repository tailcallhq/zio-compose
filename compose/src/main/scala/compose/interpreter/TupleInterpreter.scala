package compose.interpreter

import compose.operation.TupleOp
import zio.schema.DynamicValue
import zio.{Task, ZIO}

trait TupleInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalTuple(input: DynamicValue, operation: TupleOp): Task[DynamicValue] = {
    operation match {
      case TupleOp.Arg(plan, i) =>
        for {
          input  <- evalDynamic(plan, input)
          result <- input match {
            case DynamicValue.Tuple(left, right) =>
              i match {
                case 0 => ZIO.succeed(left)
                case 1 => ZIO.succeed(right)
                case n =>
                  ZIO.fail(
                    new RuntimeException(s"Can not extract element at index ${n} from ${input}"),
                  )
              }
            case _ => ZIO.fail(new RuntimeException(s"Can not extract args from ${input}"))
          }
        } yield result
    }
  }
}
