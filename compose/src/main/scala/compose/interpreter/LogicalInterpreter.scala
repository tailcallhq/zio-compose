package compose.interpreter

import compose.operation.LogicalOp
import zio.schema.DynamicValue
import zio.Task

trait LogicalInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalLogical(input: DynamicValue, operation: LogicalOp): Task[DynamicValue] = {
    operation match {
      case LogicalOp.And(left, right) =>
        for {
          left  <- eval[Boolean](left, input)
          right <- eval[Boolean](right, input)
        } yield toDynamic {
          left && right
        }
      case LogicalOp.Or(left, right)  =>
        for {
          left  <- eval[Boolean](left, input)
          right <- eval[Boolean](right, input)
        } yield toDynamic {
          left || right
        }

      case LogicalOp.Not(plan) =>
        for {
          bool <- eval[Boolean](plan, input)
        } yield toDynamic(!bool)

      case LogicalOp.Equals(left, right) =>
        for {
          left  <- evalDynamic(left, input)
          right <- evalDynamic(right, input)
        } yield toDynamic(left == right)

      case LogicalOp.Diverge(cond, ifTrue, ifFalse) =>
        for {
          cond   <- eval[Boolean](cond, input)
          result <- if (cond) evalDynamic(ifTrue, input) else evalDynamic(ifFalse, input)
        } yield result
    }
  }
}
