package com.tusharmath.compose

import zio.prelude.NonEmptyList
import zio.schema.Schema

sealed trait Lambda[-A, +B] { self =>
  final def >>>[C](other: Lambda[B, C]): Lambda[A, C] = self pipe other

  final def <<<[X](other: Lambda[X, A]): Lambda[X, B] = self compose other

  final def compose[X](other: Lambda[X, A]): Lambda[X, B] =
    Lambda.Pipe(other, self)

  final def pipe[C](other: Lambda[B, C]): Lambda[A, C] =
    Lambda.Pipe(self, other)

  final def zip[A1 <: A, A2, B1 >: B, B2](
    other: Lambda[A2, B2],
  )(implicit
    b1: Schema[B1],
    b2: Schema[B2],
  ): (A1, A2) ~> (B1, B2) =
    Lambda.zip(self: A1 ~> B1, other)

  private[compose] final def compile: ExecutionPlan =
    ExecutionPlan.fromLambda(self)
}

object Lambda {

  def F: Any ~> Boolean = always(false)

  def T: Any ~> Boolean = always(true)

  def add: (Int, Int) ~> Int = AddInt

  def always[B](a: B)(implicit schema: Schema[B]): Any ~> B =
    Always(a, schema)

  def and: (Boolean, Boolean) ~> Boolean = LogicalAnd

  def apply[B](a: B)(implicit schema: Schema[B]): Any ~> B = always(a)

  def dec: Int ~> Int = ???

  def eq[A]: (A, A) ~> Boolean = EqualTo()

  def fromMap[A, B](
    source: Map[A, B],
  )(implicit input: Schema[A], output: Schema[B]): Lambda[A, B] =
    FromMap(input, source, output)

  def gt: (Int, Int) ~> Boolean = GreaterThanInt

  def gte: (Int, Int) ~> Boolean = GreaterThanEqualInt

  def identity[A]: Lambda[A, A] = Identity[A]()

  def ifElse[A, B](f: A ~> Boolean)(isTrue: A ~> B, isFalse: A ~> B): A ~> B =
    IfElse(f, isTrue, isFalse)

  def inc: Int ~> Int = ???

  def lt: (Int, Int) ~> Boolean = gt >>> not

  def lte: (Int, Int) ~> Boolean = not <<< gte

  def mul: (Int, Int) ~> Int = MulInt

  def not: Boolean ~> Boolean = LogicalNot

  def or: (Boolean, Boolean) ~> Boolean = LogicalOr

  def zip[A1, A2, B1, B2](f1: A1 ~> B1, f2: A2 ~> B2)(implicit
    b1: Schema[B1],
    b2: Schema[B2],
  ): (A1, A2) ~> (B1, B2) =
    Zip2(f1, f2, b1, b2)

  final case class EqualTo[A]() extends Lambda[(A, A), Boolean]

  final case class FromMap[A, B](
    input: Schema[A],
    source: Map[A, B],
    output: Schema[B],
  ) extends Lambda[A, B]

  final case class Always[B](b: B, schema: Schema[B]) extends Lambda[Any, B]

  final case class Identity[A]() extends Lambda[A, A]

  final case class Pipe[A, B, C](f: Lambda[A, B], g: Lambda[B, C])
      extends Lambda[A, C]

  final case class Zip2[A1, A2, B1, B2](
    f1: A1 ~> B1,
    f2: A2 ~> B2,
    o1: Schema[B1],
    o2: Schema[B2],
  ) extends Lambda[(A1, A2), (B1, B2)]

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

  case object LogicalNot extends Lambda[Boolean, Boolean]
  case object LogicalAnd extends Lambda[(Boolean, Boolean), Boolean]
  case object LogicalOr  extends Lambda[(Boolean, Boolean), Boolean]

  case object GreaterThanInt extends Lambda[(Int, Int), Boolean]

  case object GreaterThanEqualInt extends Lambda[(Int, Int), Boolean]

  final case object AddInt extends Lambda[(Int, Int), Int]

  final case object MulInt extends Lambda[(Int, Int), Int]
}
