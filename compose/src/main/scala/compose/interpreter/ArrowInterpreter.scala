package compose.interpreter

import compose.operation.ArrowOp
import zio.schema.DynamicValue
import zio.{Task, ZIO}

trait ArrowInterpreter {
  self: InMemoryInterpreter =>
  def evalArrow(input: DynamicValue, operation: ArrowOp): Task[DynamicValue] = {
    operation match {
      case ArrowOp.Zip(left, right) =>
        for {
          a <- evalDynamic(left, input)
          b <- evalDynamic(right, input)
        } yield DynamicValue.Tuple(a, b)

      case ArrowOp.Pipe(first, second) =>
        for {
          input  <- evalDynamic(first, input)
          output <- evalDynamic(second, input)
        } yield output

      case ArrowOp.Identity => ZIO.succeed(input)
    }
  }
}
