package compose.operation

import compose.ExecutionPlan

sealed trait NumericOp
object NumericOp {
  sealed trait Kind

  object Kind {
    case object IntNumber extends Kind
  }

  final case class Add(left: ExecutionPlan, right: ExecutionPlan) extends NumericOp

  final case class Multiply(left: ExecutionPlan, right: ExecutionPlan) extends NumericOp

  final case class Divide(left: ExecutionPlan, right: ExecutionPlan) extends NumericOp

  final case class GreaterThan(left: ExecutionPlan, right: ExecutionPlan) extends NumericOp

  final case class GreaterThanEqualTo(left: ExecutionPlan, right: ExecutionPlan) extends NumericOp

  final case class Negate(plan: ExecutionPlan) extends NumericOp
}
