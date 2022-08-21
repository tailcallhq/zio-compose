package compose.interpreter

import compose.ExecutionPlan.LogicalExecution
import zio.schema.DynamicValue
import zio.Task

trait LogicalInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalLogical(input: DynamicValue, operation: LogicalExecution.Operation): Task[DynamicValue] = {
    operation match {
      case LogicalExecution.And(left, right) =>
        for {
          left  <- eval[Boolean](left, input)
          right <- eval[Boolean](right, input)
        } yield toDynamic {
          left && right
        }
      case LogicalExecution.Or(left, right)  =>
        for {
          left  <- eval[Boolean](left, input)
          right <- eval[Boolean](right, input)
        } yield toDynamic {
          left || right
        }

      case LogicalExecution.Not(plan) =>
        for {
          bool <- eval[Boolean](plan, input)
        } yield toDynamic(!bool)

      case LogicalExecution.Equals(left, right) =>
        for {
          left  <- evalDynamic(left, input)
          right <- evalDynamic(right, input)
        } yield toDynamic(left == right)

      case LogicalExecution.Diverge(cond, ifTrue, ifFalse) =>
        for {
          cond   <- eval[Boolean](cond, input)
          result <- if (cond) evalDynamic(ifTrue, input) else evalDynamic(ifFalse, input)
        } yield result
    }
  }
}
