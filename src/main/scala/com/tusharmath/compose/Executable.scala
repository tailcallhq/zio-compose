package com.tusharmath.compose
import zio.schema.{DeriveSchema, DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.{Chunk, Task, ZIO}

sealed trait Executable { self =>
  def binary: Chunk[Byte] = JsonCodec.encode(Executable.schema)(self)

  def execute: Task[DynamicValue] =
    ScalaExecutor.execute(self, unit)

  def executeWith(dynamicValue: DynamicValue): Task[DynamicValue] =
    ScalaExecutor.execute(self, dynamicValue)

  def json: String = new String(binary.toArray)

  def unit: DynamicValue = Schema[Unit].toDynamic(())
}

object Executable {

  def fromJson(json: String): ZIO[Any, Exception, Executable] =
    JsonCodec.decode(Executable.schema)(
      Chunk.fromArray(json.getBytes),
    ) match {
      case Left(value)  => ZIO.fail(new Exception(value))
      case Right(value) => ZIO.succeed(value)
    }

  final case class And(left: Executable, right: Executable) extends Executable

  final case class Literal(a: DynamicValue) extends Executable

  final case class AddInteger(a: Executable, b: Executable) extends Executable

  final case class IfElse(
    condition: Executable,
    ifTrue: Executable,
    ifFalse: Executable,
  ) extends Executable

  final case class ExecMap(
    r: Executable,
    f: Executable,
  ) extends Executable

  final case class GreaterThan(
    first: Executable,
    second: Executable,
  ) extends Executable

  case object Length extends Executable

  case object UpperCase extends Executable

  implicit def schema = DeriveSchema.gen[Executable]
}
