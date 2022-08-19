package compose.operation

import compose.ExecutionPlan

sealed trait ArrowOp
object ArrowOp {

  final case class Zip(left: ExecutionPlan, right: ExecutionPlan) extends ArrowOp

  final case class Pipe(first: ExecutionPlan, second: ExecutionPlan) extends ArrowOp

  case object Identity extends ArrowOp
}
