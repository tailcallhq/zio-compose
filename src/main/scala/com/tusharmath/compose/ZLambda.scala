package com.tusharmath.compose

import com.tusharmath.compose.ZLambda.{Pipe, Zip2}
import zio.prelude.NonEmptyList
import zio.schema.{AccessorBuilder, Schema}

sealed trait ZLambda[A, B] { self =>
  def <<<[X](other: ZLambda[X, A]): ZLambda[X, B] = self compose other

  def compose[X](other: ZLambda[X, A]): ZLambda[X, B] = Pipe(other, self)

  def >>>[C](other: ZLambda[B, C]): ZLambda[A, C] = self pipe other

  def pipe[C](other: ZLambda[B, C]): ZLambda[A, C] = Pipe(self, other)

  def &&[C](other: ZLambda[A, C])(implicit s1: Schema[B], s2: Schema[C]): ZLambda[A, (B, C)] =
    self zip other

  def zip[C](other: ZLambda[A, C])(implicit s1: Schema[B], s2: Schema[C]): ZLambda[A, (B, C)] =
    Zip2(self, other, s1, s2)

  def executable: ExecutionPlan = ExecutionPlan.fromGraphQL(self)
}

object ZLambda {
  def fromMap[A, B](source: Map[A, B])(implicit input: Schema[A], output: Schema[B]): ZLambda[A, B] =
    FromMap(input, source, output)

  def add: (Int, Int) ~> Int = AddInt

  def identity[A]: ZLambda[A, A] = Identity[A]()

  def useWith[A, A1, A2, B](
    f: (A1, A2) ~> B,
  )(f1: A ~> A1, f2: A ~> A2)(implicit s1: Schema[A1], s2: Schema[A2]): A ~> B = zip(f1, f2) >>> f

  def zip[A, B1, B2](f1: A ~> B1, f2: A ~> B2)(implicit s1: Schema[B1], s2: Schema[B2]): A ~> (B1, B2) =
    Zip2(f1, f2, s1, s2)

  def unit: ZLambda[Unit, Unit] = ZLambda(())

  def apply[B](a: B)(implicit schema: Schema[B]): ZLambda[Unit, B] = constant(a)

  def constant[B](a: B)(implicit schema: Schema[B]): ZLambda[Unit, B] = Constant(a, schema)

  final case class FromMap[A, B](input: Schema[A], source: Map[A, B], output: Schema[B]) extends ZLambda[A, B]
  final case class Constant[B](b: B, schema: Schema[B])                                  extends ZLambda[Unit, B]
  final case class Identity[A]()                                                         extends ZLambda[A, A]
  final case class Pipe[A, B, C](f: ZLambda[A, B], g: ZLambda[B, C])                     extends ZLambda[A, C]
  final case class Zip2[A, B1, B2](f1: A ~> B1, f2: A ~> B2, s1: Schema[B1], s2: Schema[B2])
      extends ZLambda[A, (B1, B2)]
  final case class Select[A, B](input: Schema[A], path: NonEmptyList[String], output: Schema[B]) extends ZLambda[A, B]
  final case object AddInt extends ZLambda[(Int, Int), Int]

  object Accessors extends AccessorBuilder {
    override type Lens[S, A]      = ZLambda[S, A]
    override type Prism[S, A]     = Unit
    override type Traversal[S, A] = Unit

    override def makeLens[S, A](product: Schema.Record[S], term: Schema.Field[A]): Lens[S, A] =
      ZLambda.Select(product, NonEmptyList(term.label), term.schema)

    override def makePrism[S, A](sum: Schema.Enum[S], term: Schema.Case[A, S]): Prism[S, A] = ()

    override def makeTraversal[S, A](collection: Schema.Collection[S, A], element: Schema[A]): Traversal[S, A] = ()
  }
}
