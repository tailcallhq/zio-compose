package compose.interpreter

import compose.ExecutionPlan.ArrowExecution
import zio.schema.DynamicValue
import zio.{Task, ZIO}

trait ArrowInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalArrow(input: DynamicValue, operation: ArrowExecution.Operation): Task[DynamicValue] = {
    operation match {
      case ArrowExecution.Zip(left, right) =>
        for {
          a <- evalDynamic(left, input)
          b <- evalDynamic(right, input)
        } yield DynamicValue.Tuple(a, b)

      case ArrowExecution.Pipe(first, second) =>
        for {
          input  <- evalDynamic(first, input)
          output <- evalDynamic(second, input)
        } yield output

      case ArrowExecution.Identity => ZIO.succeed(input)
    }
  }
}
