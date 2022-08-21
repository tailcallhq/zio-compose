package compose.interpreter

import compose.ExecutionPlan.RecursiveExecution
import zio.schema.DynamicValue
import zio.{Task, ZIO}

trait RecursiveInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalRecursive(input: DynamicValue, operation: RecursiveExecution.Operation): Task[DynamicValue] = {
    operation match {
      case RecursiveExecution.RepeatWhile(f, cond) =>
        def loop(input: DynamicValue): Task[DynamicValue] = {
          for {
            output <- evalDynamic(f, input)
            isTrue <- eval[Boolean](cond, output)
            result <- if (isTrue) loop(output) else ZIO.succeed(output)
          } yield result
        }

        loop(input)

      case RecursiveExecution.DoWhile(f, cond) =>
        def loop: Task[DynamicValue] = {
          for {
            output <- evalDynamic(f, input)
            isTrue <- eval[Boolean](cond, input)
            result <- if (isTrue) loop else ZIO.succeed(output)
          } yield result
        }

        loop
    }
  }
}
