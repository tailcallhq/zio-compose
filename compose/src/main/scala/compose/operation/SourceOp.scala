package compose.operation

import zio.schema.DynamicValue

sealed trait SourceOp
object SourceOp {

  final case class Default(value: DynamicValue) extends SourceOp

  final case class FromMap(value: Map[DynamicValue, DynamicValue]) extends SourceOp

  final case class Constant(value: DynamicValue) extends SourceOp

  final case object WriteLine extends SourceOp
}
