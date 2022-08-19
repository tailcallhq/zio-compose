package compose.operation

import compose.ExecutionPlan

sealed trait StringOp
object StringOp {

  final case class StartsWith(self: ExecutionPlan, other: ExecutionPlan) extends StringOp

  final case class EndsWith(self: ExecutionPlan, other: ExecutionPlan) extends StringOp

  final case class Contains(self: ExecutionPlan, other: ExecutionPlan) extends StringOp

  final case class Length(self: ExecutionPlan) extends StringOp

  final case class UpperCase(self: ExecutionPlan) extends StringOp

  final case class LowerCase(self: ExecutionPlan) extends StringOp

  final case class Concat(self: ExecutionPlan, other: ExecutionPlan) extends StringOp
}
