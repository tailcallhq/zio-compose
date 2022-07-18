package com.tusharmath.compose

import com.tusharmath.compose.GraphQL.{Pipe, Zip2}
import zio.schema.{AccessorBuilder, DynamicValue, Schema}
import zio.prelude.NonEmptyList

sealed trait GraphQL[A, B] { self =>
  def <<<[X](other: GraphQL[X, A]): GraphQL[X, B] = self compose other
  def >>>[C](other: GraphQL[B, C]): GraphQL[A, C] = self pipe other

  def compose[X](other: GraphQL[X, A]): GraphQL[X, B]  = Pipe(other, self)
  def pipe[C](other: GraphQL[B, C]): GraphQL[A, C]     = Pipe(self, other)
  def zip[C](other: GraphQL[A, C]): GraphQL[A, (B, C)] = Zip2(self, other)
  def &&[C](other: GraphQL[A, C]): GraphQL[A, (B, C)]  = self zip other
}

object GraphQL {
  def constant[B](a: B)(implicit schema: Schema[B]): GraphQL[Unit, B] = Constant(a, schema)

  def fromMap[A, B](source: Map[A, B])(implicit input: Schema[A], output: Schema[B]): GraphQL[A, B] =
    FromMap(input, source, output)

  final case class FromMap[A, B](input: Schema[A], source: Map[A, B], output: Schema[B]) extends GraphQL[A, B]
  final case class Constant[B](b: B, schema: Schema[B])                                  extends GraphQL[Unit, B]
  final case class Identity[A]()                                                         extends GraphQL[A, A]
  final case class Pipe[A, B, C](f: GraphQL[A, B], g: GraphQL[B, C])                     extends GraphQL[A, C]
  final case class Zip2[A, B, C](g: GraphQL[A, B], f: GraphQL[A, C])                     extends GraphQL[A, (B, C)]
  final case class Select[A, B](input: Schema[A], path: NonEmptyList[String], output: Schema[B]) extends GraphQL[A, B]

  def execute[A, B](graphQL: GraphQL[A, B])(a: A): Either[String, B] =
    graphQL match {
      case Constant(b, _)     => Right(b)
      case Identity()         => Right(a.asInstanceOf[B])
      case Pipe(f, g)         => execute(f)(a).flatMap(execute(g))
      case FromMap(_, map, _) => map.get(a).toRight("No value found for " + a)
      case Zip2(g, f)         =>
        for {
          a <- execute(g)(a)
          b <- execute(f)(a)
        } yield (a, b)

      case Select(input, path, output) =>
        input.toDynamic(a) match {
          case record @ DynamicValue.Record(_) => Left("TODO")
          case _                               => Left(s"Cannot select field}")
        }
    }

  object Accessors extends AccessorBuilder {
    override type Lens[S, A]      = GraphQL[S, A]
    override type Prism[S, A]     = Unit
    override type Traversal[S, A] = Unit

    override def makeLens[S, A](product: Schema.Record[S], term: Schema.Field[A]): Lens[S, A] =
      GraphQL.Select(product, NonEmptyList(term.label), term.schema)

    override def makePrism[S, A](sum: Schema.Enum[S], term: Schema.Case[A, S]): Prism[S, A] = ()

    override def makeTraversal[S, A](collection: Schema.Collection[S, A], element: Schema[A]): Traversal[S, A] = ()
  }
}
