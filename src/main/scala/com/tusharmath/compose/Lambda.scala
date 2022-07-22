package com.tusharmath.compose

import com.tusharmath.compose.Lambda.converge1
import zio.prelude.NonEmptyList
import zio.schema.Schema

sealed trait Lambda[A, B] { self =>
  final def <<<[X](other: Lambda[X, A]): Lambda[X, B] = self compose other

  final def compose[X](other: Lambda[X, A]): Lambda[X, B] =
    Lambda.Pipe(other, self)

  final def >>:(a: A)(implicit schema: Schema[A]): Unit ~> B = call(a)

  final def call(a: A)(implicit schema: Schema[A]): Unit ~> B =
    Lambda(a) >>> self

  final def >>>[C](other: Lambda[B, C]): Lambda[A, C] = self pipe other

  final def pipe[C](other: Lambda[B, C]): Lambda[A, C] =
    Lambda.Pipe(self, other)

  final def unary[A1](implicit
    ev: A =:= (A1, A1),
    schema: Schema[A1],
  ): A1 ~> B = converge1(self.asInstanceOf[(A1, A1) ~> B])(
    Lambda.identity,
    Lambda.identity,
  )

  final def <*>[A1, B1](
    other: Lambda[A1, B1],
  )(implicit
    a1: Schema[A],
    a2: Schema[A1],
    b1: Schema[B],
    b2: Schema[B1],
  ): (A, A1) ~> (B, B1) = self zip other

  final def zip[A1, B1](
    other: Lambda[A1, B1],
  )(implicit
    b1: Schema[B],
    b2: Schema[B1],
  ): (A, A1) ~> (B, B1) =
    Lambda.zip(self, other)

  private[compose] final def executable: ExecutionPlan =
    ExecutionPlan.fromLambda(self)
}

object Lambda {

  def unary[A, B](f: (A, A) ~> B)(implicit a: Schema[A]): A ~> B =
    converge1(f)(identity, identity)

  def converge1[A, A1, A2, C](
    f: (A1, A2) ~> C,
  )(f1: A ~> A1, f2: A ~> A2)(implicit
    a1: Schema[A1],
    a2: Schema[A2],
  ): A ~> C =
    Converge1(f, f1, f2, a1, a2)

  def identity[A]: Lambda[A, A] = Identity[A]()

  def lt: (Int, Int) ~> Boolean = gt >>> not

  def not: Boolean ~> Boolean = LogicalNot

  def gt: (Int, Int) ~> Boolean = GreaterThanInt

  def gte: (Int, Int) ~> Boolean = GreaterThanEqualInt

  def eq[A]: (A, A) ~> Boolean = EqualTo()

  def and: (Boolean, Boolean) ~> Boolean = LogicalAnd

  def or: (Boolean, Boolean) ~> Boolean = LogicalOr

  def converge2[A, B, A1, A2, C](
    f: (A1, A2) ~> C,
  )(f1: (A, B) ~> A1, f2: (A, B) ~> A2)(implicit
    a1: Schema[A1],
    a2: Schema[A2],
  ): (A, B) ~> C = Converge2(f, f1, f2, a1, a2)

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

  def useWith[A1, B1, A2, B2, B](f: (B1, B2) ~> B)(f1: A1 ~> B1, f2: A2 ~> B2)(
    implicit
    a1: Schema[A1],
    a2: Schema[A2],
    b1: Schema[B1],
    b2: Schema[B2],
  ): (A1, A2) ~> B =
    (f1 zip f2) >>> f

  def T: Unit ~> Boolean = always(true)

  def F: Unit ~> Boolean = always(false)

  def always[B](a: B)(implicit schema: Schema[B]): Lambda[Unit, B] =
    Always(a, schema)

  def zip[A1, A2, B1, B2](f1: A1 ~> B1, f2: A2 ~> B2)(implicit
    b1: Schema[B1],
    b2: Schema[B2],
  ): (A1, A2) ~> (B1, B2) =
    Zip2(f1, f2, b1, b2)

  def unit: Lambda[Unit, Unit] = Lambda(())

  def apply[B](a: B)(implicit schema: Schema[B]): Lambda[Unit, B] = always(a)

  final case class Converge2[A, B, A1, A2, C](
    f: (A1, A2) ~> C,
    f1: (A, B) ~> A1,
    f2: (A, B) ~> A2,
    s1: Schema[A1],
    s2: Schema[A2],
  ) extends Lambda[(A, B), C]

  final case class Converge1[A, A1, A2, C](
    f: (A1, A2) ~> C,
    f1: A ~> A1,
    f2: A ~> A2,
    s1: Schema[A1],
    s2: Schema[A2],
  ) extends Lambda[A, C]

  final case class EqualTo[A]() extends Lambda[(A, A), Boolean]

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

  case object LogicalNot extends Lambda[Boolean, Boolean]
  case object LogicalAnd extends Lambda[(Boolean, Boolean), Boolean]
  case object LogicalOr  extends Lambda[(Boolean, Boolean), Boolean]

  case object GreaterThanInt extends Lambda[(Int, Int), Boolean]

  case object GreaterThanEqualInt extends Lambda[(Int, Int), Boolean]

  final case object AddInt extends Lambda[(Int, Int), Int]

  final case object MulInt extends Lambda[(Int, Int), Int]
}
