package compose

import compose.operation._
import zio.schema.{DeriveSchema, Schema}
import zio.schema.codec.JsonCodec
import zio.{Chunk, ZIO}

sealed trait ExecutionPlan { self =>
  final def binary: Chunk[Byte] = JsonCodec.encode(ExecutionPlan.schema)(self)
  final def json: String        = new String(binary.toArray)
}

object ExecutionPlan {

  def from(json: String): ZIO[Any, Exception, ExecutionPlan] =
    from(Chunk.fromArray(json.getBytes))

  def from(chunk: Chunk[Byte]): ZIO[Any, Exception, ExecutionPlan] =
    JsonCodec.decode(ExecutionPlan.schema)(chunk) match {
      case Left(value)  => ZIO.fail(new Exception(value))
      case Right(value) => ZIO.succeed(value)
    }

  implicit def schema: Schema[ExecutionPlan] = DeriveSchema.gen[ExecutionPlan]

  final case class ScopeExecution(operation: ScopeOp)                           extends ExecutionPlan
  final case class LogicalExecution(operation: LogicalOp)                       extends ExecutionPlan
  final case class NumericExecution(operation: NumericOp, kind: NumericOp.Kind) extends ExecutionPlan
  final case class StringExecution(operation: StringOp)                         extends ExecutionPlan
  final case class OpticalExecution(operation: OpticalOp)                       extends ExecutionPlan
  final case class ArrowExecution(operation: ArrowOp)                           extends ExecutionPlan
  final case class DebugExecution(operation: DebugOp)                           extends ExecutionPlan
  final case class TupleExecution(operation: TupleOp)                           extends ExecutionPlan
  final case class SourceExecution(operation: SourceOp)                         extends ExecutionPlan
  final case class RecursiveExecution(operation: RecursiveOp)                   extends ExecutionPlan

}
