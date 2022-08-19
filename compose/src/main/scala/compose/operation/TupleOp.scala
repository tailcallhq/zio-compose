package compose.operation

import compose.ExecutionPlan

sealed trait TupleOp
object TupleOp {

  final case class Arg(plan: ExecutionPlan, n: Int) extends TupleOp
}
