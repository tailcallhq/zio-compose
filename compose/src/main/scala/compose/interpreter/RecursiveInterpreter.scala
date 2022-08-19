package compose.interpreter

import compose.operation.RecursiveOp
import zio.schema.DynamicValue
import zio.{Task, ZIO}

trait RecursiveInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalRecursive(input: DynamicValue, operation: RecursiveOp): Task[DynamicValue] = {
    operation match {
      case RecursiveOp.RepeatWhile(f, cond) =>
        def loop(input: DynamicValue): Task[DynamicValue] = {
          for {
            output <- evalDynamic(f, input)
            isTrue <- eval[Boolean](cond, output)
            result <- if (isTrue) loop(output) else ZIO.succeed(output)
          } yield result
        }

        loop(input)

      case RecursiveOp.DoWhile(f, cond) =>
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
