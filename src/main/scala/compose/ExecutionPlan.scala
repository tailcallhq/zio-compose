package compose

import zio.schema.{DeriveSchema, DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.{Chunk, ZIO}
import zio.schema.ast.SchemaAst

sealed trait ExecutionPlan { self =>
  def binary: Chunk[Byte] = JsonCodec.encode(ExecutionPlan.schema)(self)

  def json: String = new String(binary.toArray)
}

object ExecutionPlan {

  implicit def schema: Schema[ExecutionPlan] = DeriveSchema.gen[ExecutionPlan]

  def from(json: String): ZIO[Any, Exception, ExecutionPlan] =
    from(Chunk.fromArray(json.getBytes))

  def from(chunk: Chunk[Byte]): ZIO[Any, Exception, ExecutionPlan] =
    JsonCodec.decode(ExecutionPlan.schema)(chunk) match {
      case Left(value)  => ZIO.fail(new Exception(value))
      case Right(value) => ZIO.succeed(value)
    }

  final case class SetScope(scope: Int, ctx: Int) extends ExecutionPlan

  final case class GetScope(scope: Int, ctx: Int, value: DynamicValue) extends ExecutionPlan

  final case class Arg(n: Int, ast0: SchemaAst, ast1: SchemaAst) extends ExecutionPlan

  final case class RepeatWhile(self: ExecutionPlan, cond: ExecutionPlan) extends ExecutionPlan

  final case class Concat(self: ExecutionPlan, other: ExecutionPlan, canConcat: DynamicValue) extends ExecutionPlan

  final case class Default(value: DynamicValue) extends ExecutionPlan

  final case class LogicalAnd(left: ExecutionPlan, right: ExecutionPlan) extends ExecutionPlan

  final case class LogicalOr(left: ExecutionPlan, right: ExecutionPlan) extends ExecutionPlan

  final case class LogicalNot(plan: ExecutionPlan) extends ExecutionPlan

  final case class NumericOperation(
    operation: Numeric.Operation,
    left: ExecutionPlan,
    right: ExecutionPlan,
    numeric: DynamicValue,
  ) extends ExecutionPlan

  final case class Zip(left: ExecutionPlan, right: ExecutionPlan) extends ExecutionPlan

  final case class IfElse(cond: ExecutionPlan, ifTrue: ExecutionPlan, ifFalse: ExecutionPlan) extends ExecutionPlan

  final case class Pipe(first: ExecutionPlan, second: ExecutionPlan) extends ExecutionPlan

  final case class GetPath(path: List[String]) extends ExecutionPlan

  final case class SetPath(path: List[String]) extends ExecutionPlan

  final case class Equals(left: ExecutionPlan, right: ExecutionPlan) extends ExecutionPlan

  final case class FromMap(value: Map[DynamicValue, DynamicValue], ast: SchemaAst) extends ExecutionPlan

  final case class Constant(value: DynamicValue) extends ExecutionPlan

  final case class Debug(name: String, plan: ExecutionPlan) extends ExecutionPlan

  final case class EndScope(ctx: Int) extends ExecutionPlan

  case object Identity extends ExecutionPlan
}
