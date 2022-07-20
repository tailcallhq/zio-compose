package com.tusharmath.compose

import com.tusharmath.compose.ZLambda.Pipe
import com.tusharmath.compose.ZLambda.Zip2
import zio.prelude.NonEmptyList
import zio.schema.AccessorBuilder
import zio.schema.Schema
import zio.schema.StandardType

sealed trait ZLambda[A, B] { self =>
  def <<<[X](other: ZLambda[X, A]): ZLambda[X, B] = self compose other
  def >>>[C](other: ZLambda[B, C]): ZLambda[A, C] = self pipe other

  def compose[X](other: ZLambda[X, A]): ZLambda[X, B]  = Pipe(other, self)
  def pipe[C](other: ZLambda[B, C]): ZLambda[A, C]     = Pipe(self, other)
  def zip[C](other: ZLambda[A, C]): ZLambda[A, (B, C)] = Zip2(self, other)
  def &&[C](other: ZLambda[A, C]): ZLambda[A, (B, C)]  = self zip other
}

object ZLambda {
  def constant[B](a: B)(implicit schema: Schema[B]): ZLambda[Unit, B] = Constant(a, schema)

  def fromMap[A, B](source: Map[A, B])(implicit input: Schema[A], output: Schema[B]): ZLambda[A, B] =
    FromMap(input, source, output)

  def add[A](a: A, b: A)(implicit ev: IsNumeric[A], schema: Schema[A], standard: StandardType[A]): ZLambda[Unit, A] =
    Add(a, b, schema, standard)

  final case class FromMap[A, B](input: Schema[A], source: Map[A, B], output: Schema[B]) extends ZLambda[A, B]
  final case class Constant[B](b: B, schema: Schema[B])                                  extends ZLambda[Unit, B]
  final case class Identity[A]()                                                         extends ZLambda[A, A]
  final case class Pipe[A, B, C](f: ZLambda[A, B], g: ZLambda[B, C])                     extends ZLambda[A, C]
  final case class Zip2[A, B, C](g: ZLambda[A, B], f: ZLambda[A, C])                     extends ZLambda[A, (B, C)]
  final case class Select[A, B](input: Schema[A], path: NonEmptyList[String], output: Schema[B]) extends ZLambda[A, B]
  final case class Add[A](first: A, second: A, schema: Schema[A], standardType: StandardType[A])
      extends ZLambda[Unit, A]

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
