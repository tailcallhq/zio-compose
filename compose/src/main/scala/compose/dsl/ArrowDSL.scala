package compose.dsl

import compose.{lens, ~>, Lambda}
import compose.Lambda.{make, ScopeContext}
import compose.dsl.ArrowDSL.CanConcat
import compose.execution.ExecutionPlan
import compose.interpreter.Interpreter
import compose.lens.Transformation
import zio.schema.{DeriveSchema, Schema}
import zio.Task

trait ArrowDSL[-A, +B] { self: A ~> B =>
  final def =!=[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean =
    self notEq other

  final def =:=[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean =
    self eq other

  final def >>>[C](other: B ~> C): A ~> C = self pipe other

  final def <<<[X](other: X ~> A): X ~> B = self compose other

  final def <*[A1 <: A, B1 >: B, B2](other: A1 ~> B2): A1 ~> B1 =
    (self: A1 ~> B1) zipLeft other

  final def <*>[A1 <: A, B1 >: B, B2](other: A1 ~> B2): A1 ~> (B1, B2) =
    (self: A1 ~> B1) zip other

  final def ++[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit ev: CanConcat[B1]): A1 ~> B1 =
    make[A, B] {
      ExecutionPlan.Concat(self.compile, other.compile, Schema[CanConcat[_]].toDynamic(ev))
    }

  final def *>[A1 <: A, B1 >: B, B2](other: A1 ~> B2)(implicit b1: Schema[B1], b2: Schema[B2]): A1 ~> B2 =
    (self: A1 ~> B1) zipRight other

  final def bind[A1 <: A](a: A1)(implicit ev: Schema[A1]): Any ~> B = Lambda.constant(a) >>> self

  final def compose[X](other: X ~> A): X ~> B =
    other pipe self

  final def doWhile[C](cond: C ~> Boolean): A ~> B =
    make[A, B](ExecutionPlan.DoWhile(self.compile, cond.compile))

  final def endContext[B1 >: B](ctx: ScopeContext)(implicit s: Schema[B1]): A ~> B1 =
    make[A, B1](ExecutionPlan.EndScope(self.compile, ctx.hashCode()))


  final def eval[A1 <: A, B1 >: B](a: A1)(implicit in: Schema[A1], out: Schema[B1]): Task[B1] =
    Interpreter.inMemory.flatMap(_.eval[B1](self.compile, in.toDynamic(a)))

  final def notEq[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean =
    (self =:= other).not

  final def pipe[C](other: B ~> C): A ~> C =
    make[A, C] { ExecutionPlan.Pipe(self.compile, other.compile) }

  final def repeatUntil[B1 >: B <: A](cond: B1 ~> Boolean): B1 ~> B1 =
    repeatWhile(cond.not)

  final def repeatWhile[B1 >: B <: A](cond: B1 ~> Boolean): B1 ~> B1 =
    make[B1, B1] { ExecutionPlan.RepeatWhile(self.compile, cond.compile) }

  final def show(name: String): A ~> B = make[A, B](ExecutionPlan.Show(self.compile, name))

  final def transform[I >: B, C](other: (C, I) ~> C)(implicit i: Schema[I]): Transformation[A, C] =
    lens.Transformation[A, C, I](self, other)

  final def zip[A1 <: A, B1 >: B, B2](other: A1 ~> B2): A1 ~> (B1, B2) =
    make[A1, (B1, B2)] { ExecutionPlan.Zip(self.compile, other.compile) }

  final def zipLeft[A1 <: A, B1 >: B, B2](other: A1 ~> B2): A1 ~> B1 =
    ((self: A1 ~> B1) <*> other)._1

  final def zipRight[A1 <: A, B1 >: B, B2](other: A1 ~> B2): A1 ~> B2 =
    ((self: A1 ~> B1) <*> other)._2
}

object ArrowDSL {
  sealed trait CanConcat[A] {}

  object CanConcat {
    implicit case object ConcatString extends CanConcat[String]
    implicit val schema: Schema[CanConcat[_]] = DeriveSchema.gen[CanConcat[_]]
  }
}
