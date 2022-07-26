package compose

import zio.schema.{DeriveSchema, DynamicValue, Schema}
import zio.schema.ast.SchemaAst
import zio.schema.codec.JsonCodec
import zio.{Chunk, Task, ZIO}

sealed trait ExecutionPlan { self =>
  def binary: Chunk[Byte] = JsonCodec.encode(ExecutionPlan.schema)(self)

  def json: String = new String(binary.toArray)

  def unsafeExecute(value: DynamicValue): Task[DynamicValue] =
    Interpreter.execute(self, value)
}

object ExecutionPlan {

  implicit def schema: Schema[ExecutionPlan] = DeriveSchema.gen[ExecutionPlan]

  def fromJson(json: String): ZIO[Any, Exception, ExecutionPlan] =
    JsonCodec.decode(ExecutionPlan.schema)(
      Chunk.fromArray(json.getBytes),
    ) match {
      case Left(value)  => ZIO.fail(new Exception(value))
      case Right(value) => ZIO.succeed(value)
    }

  def fromLambda[A, B](lmb: Lambda[A, B]): ExecutionPlan =
    lmb match {
      case Lambda.Pipe(f, g) => Sequence(f.compile, g.compile)

      case Lambda.Combine(f1, f2, o1, o2) =>
        Combine(f1.compile, f2.compile, o1.ast, o2.ast)

      case Lambda.FromMap(
            i: Schema[A] @unchecked,
            source: Map[A, B] @unchecked,
            o: Schema[B] @unchecked,
          ) =>
        Dictionary(source.map { case (k, v) =>
          (
            i.toDynamic(k),
            o.toDynamic(v),
          )
        })

      case Lambda.Select(_, path, _) => Select(path)

      case Lambda.Always(b: B @unchecked, schema: Schema[B] @unchecked) =>
        Always(schema.toDynamic(b))

      case Lambda.Identity() => Identity

      case Lambda.IfElse(f, isTrue, isFalse) =>
        IfElse(f.compile, isTrue.compile, isFalse.compile)

    }

  final case class Always(value: DynamicValue) extends ExecutionPlan

  final case class Combine(
    e1: ExecutionPlan,
    e2: ExecutionPlan,
    o1: SchemaAst,
    o2: SchemaAst,
  ) extends ExecutionPlan

  final case class Sequence(first: ExecutionPlan, second: ExecutionPlan)
      extends ExecutionPlan

  final case class Dictionary(value: Map[DynamicValue, DynamicValue])
      extends ExecutionPlan

  final case class Select(path: List[String]) extends ExecutionPlan

  final case class IfElse(
    condition: ExecutionPlan,
    ifTrue: ExecutionPlan,
    ifFalse: ExecutionPlan,
  ) extends ExecutionPlan

  case object EqualTo extends ExecutionPlan

  case object GreaterThanEqualInt extends ExecutionPlan

  case object GreaterThanInt extends ExecutionPlan

  case object LogicalNot extends ExecutionPlan

  case object LogicalAnd extends ExecutionPlan

  case object LogicalOr extends ExecutionPlan

  case object AddInt extends ExecutionPlan

  case object MulInt extends ExecutionPlan

  case object Identity extends ExecutionPlan
}
