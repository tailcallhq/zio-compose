package compose.operation

import compose.ExecutionPlan

sealed trait RecursiveOp
object RecursiveOp {
  final case class RepeatWhile(self: ExecutionPlan, cond: ExecutionPlan) extends RecursiveOp

  final case class DoWhile(plan: ExecutionPlan, cond: ExecutionPlan) extends RecursiveOp
}
