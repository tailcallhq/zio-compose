package com.tusharmath.compose
import zio.schema.{DeriveSchema, DynamicValue}
import zio.schema.ast.SchemaAst
import zio.schema.codec.JsonCodec
import zio.{Chunk, Task, ZIO}

sealed trait Executable { self =>
  def binary: Chunk[Byte] = JsonCodec.encode(Executable.schema)(self)

  def json: String = new String(binary.toArray)

  def unsafeExecute(value: DynamicValue): Task[DynamicValue] =
    Executor.execute(self, value)
}

object Executable {

  def fromJson(json: String): ZIO[Any, Exception, Executable] =
    JsonCodec.decode(Executable.schema)(
      Chunk.fromArray(json.getBytes),
    ) match {
      case Left(value)  => ZIO.fail(new Exception(value))
      case Right(value) => ZIO.succeed(value)
    }

  def fromLambda[A, B](lmb: Lambda[A, B]): Executable =
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

      case Lambda.LogicalNot => LogicalNot

      case Lambda.LogicalAnd => LogicalAnd

      case Lambda.LogicalOr => LogicalOr

      case Lambda.GreaterThanEqualInt => GreaterThanEqualInt

      case Lambda.GreaterThanInt => GreaterThanInt

      case Lambda.EqualTo() => EqualTo

      case Lambda.Converge1(f, f1, f2, s1, s2) =>
        Converge(
          f.executable,
          f1.executable,
          f2.executable,
          s1.ast,
          s2.ast,
        )

      case Lambda.Converge2(f, f1, f2, s1, s2) =>
        Converge(
          f.executable,
          f1.executable,
          f2.executable,
          s1.ast,
          s2.ast,
        )

    }

  final case class Always(value: DynamicValue) extends Executable

  final case class Zip2(
    e1: Executable,
    e2: Executable,
    o1: SchemaAst,
    o2: SchemaAst,
  ) extends Executable

  final case class Sequence(first: Executable, second: Executable)
      extends Executable

  final case class Dictionary(value: Map[DynamicValue, DynamicValue])
      extends Executable

  final case class Select(path: List[String]) extends Executable

  final case class Partial(
    argSchema: List[SchemaAst],
    argValues: List[DynamicValue],
  ) extends Executable

  final case class IfElse(
    condition: Executable,
    ifTrue: Executable,
    ifFalse: Executable,
  ) extends Executable

  case class Converge(
    f: Executable,
    f1: Executable,
    f2: Executable,
    a1: SchemaAst,
    a2: SchemaAst,
  ) extends Executable

  case object EqualTo extends Executable

  case object GreaterThanEqualInt extends Executable

  case object GreaterThanInt extends Executable

  case object LogicalNot extends Executable

  case object LogicalAnd extends Executable

  case object LogicalOr extends Executable

  case object AddInt extends Executable

  case object MulInt extends Executable

  case object Identity extends Executable

  implicit def schema = DeriveSchema.gen[Executable]
}
