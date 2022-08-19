package compose.operation

import compose.ExecutionPlan

sealed trait DebugOp
object DebugOp {

  final case class Debug(plan: ExecutionPlan, name: String) extends DebugOp

  final case class Show(plan: ExecutionPlan, name: String) extends DebugOp
}
