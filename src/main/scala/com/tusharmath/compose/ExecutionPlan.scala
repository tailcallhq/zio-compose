package com.tusharmath.compose
import zio.schema.{DeriveSchema, DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.{Chunk, Task, ZIO}

sealed trait ExecutionPlan { self =>
  def binary: Chunk[Byte] = JsonCodec.encode(ExecutionPlan.schema)(self)

  def execute: Task[DynamicValue] =
    ScalaExecutor.execute(self, unit)

  def executeWith(dynamicValue: DynamicValue): Task[DynamicValue] =
    ScalaExecutor.execute(self, dynamicValue)

  def json: String = new String(binary.toArray)

  def unit: DynamicValue = Schema[Unit].toDynamic(())
}

object ExecutionPlan {

  def fromJson(json: String): ZIO[Any, Exception, ExecutionPlan] =
    JsonCodec.decode(ExecutionPlan.schema)(
      Chunk.fromArray(json.getBytes),
    ) match {
      case Left(value)  => ZIO.fail(new Exception(value))
      case Right(value) => ZIO.succeed(value)
    }

  final case class And(left: ExecutionPlan, right: ExecutionPlan)
      extends ExecutionPlan

  final case class Literal(a: DynamicValue) extends ExecutionPlan

  final case class AddInteger(a: ExecutionPlan, b: ExecutionPlan)
      extends ExecutionPlan

  final case class IfElse(
    condition: ExecutionPlan,
    ifTrue: ExecutionPlan,
    ifFalse: ExecutionPlan,
  ) extends ExecutionPlan

  final case class ExecMap(
    r: ExecutionPlan,
    f: ExecutionPlan,
  ) extends ExecutionPlan

  final case class GreaterThan(
    first: ExecutionPlan,
    second: ExecutionPlan,
  ) extends ExecutionPlan

  case object Length extends ExecutionPlan

  case object UpperCase extends ExecutionPlan

  implicit def schema = DeriveSchema.gen[ExecutionPlan]
}
