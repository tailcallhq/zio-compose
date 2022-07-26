package compose

import zio.schema.{AccessorBuilder, Schema}
import zio.prelude.NonEmptyList

object LambdaAccessor extends AccessorBuilder {
  override type Lens[S, A]      = Lambda[S, A]
  override type Prism[S, A]     = Unit
  override type Traversal[S, A] = Unit

  override def makeLens[S, A](
    product: Schema.Record[S],
    term: Schema.Field[A],
  ): Lens[S, A] =
    Lambda.Select(product, NonEmptyList(term.label), term.schema)

  override def makePrism[S, A](
    sum: Schema.Enum[S],
    term: Schema.Case[A, S],
  ): Prism[S, A] = ()

  override def makeTraversal[S, A](
    collection: Schema.Collection[S, A],
    element: Schema[A],
  ): Traversal[S, A] = ()
}
