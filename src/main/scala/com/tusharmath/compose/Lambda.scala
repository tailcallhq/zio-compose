package com.tusharmath.compose

import com.tusharmath.compose.Lambda.Pipe
import zio.prelude.NonEmptyList
import zio.schema.Schema

sealed trait Lambda[A, B] { self =>
  def <<<[X](other: Lambda[X, A]): Lambda[X, B] = self compose other

  def compose[X](other: Lambda[X, A]): Lambda[X, B] = Pipe(other, self)

  def >>:(a: A)(implicit schema: Schema[A]): Unit ~> B = call(a)

  def call(a: A)(implicit schema: Schema[A]): Unit ~> B = Lambda(a) >>> self

  def >>>[C](other: Lambda[B, C]): Lambda[A, C] = self pipe other

  def pipe[C](other: Lambda[B, C]): Lambda[A, C] = Pipe(self, other)

  def &&[A1, B1](
    other: Lambda[A1, B1],
  )(implicit
    a1: Schema[A],
    a2: Schema[A1],
    b1: Schema[B],
    b2: Schema[B1],
  ): (A, A1) ~> (B, B1) = self zip other

  def zip[A1, B1](
    other: Lambda[A1, B1],
  )(implicit
    a1: Schema[A],
    a2: Schema[A1],
    b1: Schema[B],
    b2: Schema[B1],
  ): (A, A1) ~> (B, B1) =
    Lambda.zip(self, other)

  private[compose] def executable: ExecutionPlan =
    ExecutionPlan.fromLambda(self)
}

object Lambda {

  def ifElse[A, B](f: A ~> Boolean)(isTrue: A ~> B, isFalse: A ~> B): A ~> B =
    IfElse(f, isTrue, isFalse)

  def fromMap[A, B](
    source: Map[A, B],
  )(implicit input: Schema[A], output: Schema[B]): Lambda[A, B] =
    FromMap(input, source, output)

  def inc: Int ~> Int = Lambda.partial[Int, Int](1) >>> add

  def partial[A1, A2](a1: A1)(implicit
    s1: Schema[A1],
    s2: Schema[A2],
  ): A2 ~> (A1, A2) =
    Partial21(a1, s1, s2)

  def add: (Int, Int) ~> Int = AddInt

  def call[A, B](f: A ~> B, a: A)(implicit ev: Schema[A]): Unit ~> B = f.call(a)

  def dec: Int ~> Int = Lambda.partial[Int, Int](-1) >>> add

  def partial[A1, A2](a1: A1, a2: A2)(implicit
    s1: Schema[A1],
    s2: Schema[A2],
  ): Unit ~> (A1, A2) =
    Partial22(a1, a2, s1, s2)

  def mul: (Int, Int) ~> Int = MulInt

  def partial[A1](a1: A1)(implicit s1: Schema[A1]): Unit ~> A1 =
    Partial11(a1, s1)

  def identity[A]: Lambda[A, A] = Identity[A]()

  def useWith[A1, B1, A2, B2, B](f: (B1, B2) ~> B)(f1: A1 ~> B1, f2: A2 ~> B2)(
    implicit
    a1: Schema[A1],
    a2: Schema[A2],
    b1: Schema[B1],
    b2: Schema[B2],
  ): (A1, A2) ~> B =
    (f1 zip f2) >>> f

  def zip[A1, A2, B1, B2](f1: A1 ~> B1, f2: A2 ~> B2)(implicit
    a1: Schema[A1],
    a2: Schema[A2],
    b1: Schema[B1],
    b2: Schema[B2],
  ): (A1, A2) ~> (B1, B2) =
    Zip2(f1, f2, a1, a2, b1, b2)

  def unit: Lambda[Unit, Unit] = Lambda(())

  def apply[B](a: B)(implicit schema: Schema[B]): Lambda[Unit, B] = always(a)

  def always[B](a: B)(implicit schema: Schema[B]): Lambda[Unit, B] =
    Always(a, schema)

  final case class FromMap[A, B](
    input: Schema[A],
    source: Map[A, B],
    output: Schema[B],
  ) extends Lambda[A, B]

  final case class Always[B](b: B, schema: Schema[B]) extends Lambda[Unit, B]

  final case class Identity[A]() extends Lambda[A, A]

  final case class Pipe[A, B, C](f: Lambda[A, B], g: Lambda[B, C])
      extends Lambda[A, C]

  final case class Zip2[A1, A2, B1, B2](
    f1: A1 ~> B1,
    f2: A2 ~> B2,
    i1: Schema[A1],
    i2: Schema[A2],
    o1: Schema[B1],
    o2: Schema[B2],
  ) extends Lambda[(A1, A2), (B1, B2)]

  final case class Select[A, B](
    input: Schema[A],
    path: NonEmptyList[String],
    output: Schema[B],
  ) extends Lambda[A, B]

  final case class Partial11[A1](a1: A1, s1: Schema[A1])
      extends Lambda[Unit, A1]

  final case class Partial21[A1, A2](
    a1: A1,
    s1: Schema[A1],
    s2: Schema[A2],
  ) extends Lambda[A2, (A1, A2)]

  final case class Partial22[A1, A2](
    a1: A1,
    a2: A2,
    s1: Schema[A1],
    s2: Schema[A2],
  ) extends Lambda[Unit, (A1, A2)]

  final case class IfElse[A, B](
    f: A ~> Boolean,
    isTrue: A ~> B,
    isFalse: A ~> B,
  ) extends Lambda[A, B]

  final case object AddInt extends Lambda[(Int, Int), Int]

  final case object MulInt extends Lambda[(Int, Int), Int]
}
