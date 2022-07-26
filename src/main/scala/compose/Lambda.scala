package compose

import zio.prelude.NonEmptyList
import zio.schema.Schema

sealed trait Lambda[-A, +B] { self =>
  final def >>>[C](other: Lambda[B, C]): Lambda[A, C] = self pipe other

  final def <<<[X](other: Lambda[X, A]): Lambda[X, B] = self compose other

  final def ++[A1 <: A, B1 >: B, B2](
    other: Lambda[A1, B2],
  )(implicit
    b1: Schema[B1],
    b2: Schema[B2],
  ): A1 ~> (B1, B2) =
    (self: A1 ~> B1) zip other

  final def add[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    ev: Numeric.Is[B1],
  ): A1 ~> B1 =
    ???

  final def compose[X](other: Lambda[X, A]): Lambda[X, B] =
    Lambda.Pipe(other, self)

  final def equals[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> B1 =
    ???

  final def pipe[C](other: Lambda[B, C]): Lambda[A, C] =
    Lambda.Pipe(self, other)

  final def zip[A1 <: A, B1 >: B, B2](other: Lambda[A1, B2])(implicit
    b1: Schema[B1],
    b2: Schema[B2],
  ): A1 ~> (B1, B2) = Lambda.Combine(self, other, b1, b2)

  private[compose] final def compile: ExecutionPlan =
    ExecutionPlan.fromLambda(self)

}

object Lambda {

  def constant[B](a: B)(implicit schema: Schema[B]): Any ~> B =
    Always(a, schema)

  def fromMap[A, B](
    source: Map[A, B],
  )(implicit input: Schema[A], output: Schema[B]): Lambda[A, B] =
    FromMap(input, source, output)

  def identity[A]: Lambda[A, A] = Identity[A]()

  def ifElse[A, B](f: A ~> Boolean)(isTrue: A ~> B, isFalse: A ~> B): A ~> B =
    IfElse(f, isTrue, isFalse)

  final case class FromMap[A, B](
    input: Schema[A],
    source: Map[A, B],
    output: Schema[B],
  ) extends Lambda[A, B]

  final case class Always[B](b: B, schema: Schema[B]) extends Lambda[Any, B]

  final case class Identity[A]() extends Lambda[A, A]

  final case class Pipe[A, B, C](f: Lambda[A, B], g: Lambda[B, C])
      extends Lambda[A, C]

  final case class Select[A, B](
    input: Schema[A],
    path: NonEmptyList[String],
    output: Schema[B],
  ) extends Lambda[A, B]

  final case class IfElse[A, B](
    f: A ~> Boolean,
    isTrue: A ~> B,
    isFalse: A ~> B,
  ) extends Lambda[A, B]

  final case class Combine[A, B1, B2](
    left: A ~> B1,
    right: A ~> B2,
    o1: Schema[B1],
    o2: Schema[B2],
  ) extends Lambda[A, (B1, B2)]

}
