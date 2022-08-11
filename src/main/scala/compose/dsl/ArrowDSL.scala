package compose.dsl

import compose.{~>, CanConcat, ExecutionPlan, Lambda}
import zio.schema.Schema

trait ArrowDSL[-A, +B] { self: Lambda[A, B] =>
  final def ===[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean =
    ExecutionPlan.Equals(self.compile, other.compile).decompile

  final def >>>[C](other: Lambda[B, C]): Lambda[A, C] = self pipe other

  final def <<<[X](other: Lambda[X, A]): Lambda[X, B] = self compose other

  final def <*>[A1 <: A, B1 >: B, B2](other: Lambda[A1, B2])(implicit b1: Schema[B1], b2: Schema[B2]): A1 ~> (B1, B2) =
    (self: A1 ~> B1) zip other

  final def ++[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit ev: CanConcat[B1]): A1 ~> B1 =
    ExecutionPlan.Concat(self.compile, other.compile, Schema[CanConcat[_]].toDynamic(ev)).decompile

  final def *>[A1 <: A, B1 >: B, B2](other: Lambda[A1, B2])(implicit b1: Schema[B1], b2: Schema[B2]): A1 ~> B2 =
    ((self: A1 ~> B1) <*> other) >>> Lambda.arg1

  final def bind[A1 <: A](a: A1)(implicit ev: Schema[A1]): Any ~> B = Lambda.constant(a) >>> self

  final def compose[X](other: Lambda[X, A]): Lambda[X, B] =
    other pipe self

  final def pipe[C](other: Lambda[B, C]): Lambda[A, C] =
    ExecutionPlan.Pipe(self.compile, other.compile).decompile

  final def zip[A1 <: A, B1 >: B, B2](other: Lambda[A1, B2])(implicit
    b1: Schema[B1],
    b2: Schema[B2],
  ): A1 ~> (B1, B2) = ExecutionPlan.Combine(self.compile, other.compile, b1.ast, b2.ast).decompile
}
