package compose.operation

import compose.ExecutionPlan

sealed trait LogicalOp
object LogicalOp {

  final case class And(left: ExecutionPlan, right: ExecutionPlan) extends LogicalOp

  final case class Or(left: ExecutionPlan, right: ExecutionPlan) extends LogicalOp

  final case class Not(plan: ExecutionPlan) extends LogicalOp

  final case class Equals(left: ExecutionPlan, right: ExecutionPlan) extends LogicalOp

  final case class Diverge(cond: ExecutionPlan, ifTrue: ExecutionPlan, ifFalse: ExecutionPlan) extends LogicalOp
}
