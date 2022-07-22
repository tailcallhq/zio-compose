package com.tusharmath.compose
import zio.schema.{DeriveSchema, DynamicValue}
import zio.schema.ast.SchemaAst
import zio.schema.codec.JsonCodec
import zio.{Chunk, Task, ZIO}

sealed trait ExecutionPlan { self =>
  def json: String        = new String(binary.toArray)
  def binary: Chunk[Byte] = JsonCodec.encode(ExecutionPlan.schema)(self)
  def unsafeExecute(value: DynamicValue): Task[DynamicValue] =
    Executor.execute(self, value)
}

object ExecutionPlan {

  def fromJson(json: String): ZIO[Any, Exception, ExecutionPlan] =
    JsonCodec.decode(ExecutionPlan.schema)(
      Chunk.fromArray(json.getBytes),
    ) match {
      case Left(value)  => ZIO.fail(new Exception(value))
      case Right(value) => ZIO.succeed(value)
    }

  def fromLambda[A, B](lmb: Lambda[A, B]): ExecutionPlan =
    lmb match {

      case Lambda.Pipe(f, g) => Sequence(f.executable, g.executable)

      case Lambda.Zip2(f1, f2, o1, o2) =>
        Zip2(f1.executable, f2.executable, o1.ast, o2.ast)

      case Lambda.FromMap(i, source, o) =>
        Dictionary(source.map { case (k, v) =>
          (i.toDynamic(k), o.toDynamic(v))
        })

      case Lambda.Select(input, path, output) => Select(path)

      case Lambda.Always(b, schema) => Always(schema.toDynamic(b))

      case Lambda.Identity() => Identity

      case Lambda.AddInt => AddInt

      case Lambda.MulInt => MulInt

      case Lambda.Partial11(a1, s1) =>
        Partial(List(s1.ast), List(s1.toDynamic(a1)))

      case Lambda.Partial21(a1, s1, s2) =>
        Partial(List(s1.ast, s2.ast), List(s1.toDynamic(a1)))

      case Lambda.Partial22(a1, a2, s1, s2) =>
        Partial(
          List(s1.ast, s2.ast),
          List(s1.toDynamic(a1), s2.toDynamic(a2)),
        )

      case Lambda.IfElse(f, isTrue, isFalse) =>
        IfElse(f.executable, isTrue.executable, isFalse.executable)
    }

  case class Always(value: DynamicValue) extends ExecutionPlan

  case class Zip2(
    e1: ExecutionPlan,
    e2: ExecutionPlan,
    o1: SchemaAst,
    o2: SchemaAst,
  ) extends ExecutionPlan

  case class Sequence(first: ExecutionPlan, second: ExecutionPlan)
      extends ExecutionPlan

  case class Dictionary(value: Map[DynamicValue, DynamicValue])
      extends ExecutionPlan

  case class Select(path: List[String]) extends ExecutionPlan

  case class Partial(
    argSchema: List[SchemaAst],
    argValues: List[DynamicValue],
  ) extends ExecutionPlan

  case class IfElse(
    condition: ExecutionPlan,
    ifTrue: ExecutionPlan,
    ifFalse: ExecutionPlan,
  ) extends ExecutionPlan

  case object AddInt extends ExecutionPlan

  case object MulInt extends ExecutionPlan

  case object Identity extends ExecutionPlan

  implicit val schema = DeriveSchema.gen[ExecutionPlan]
}
