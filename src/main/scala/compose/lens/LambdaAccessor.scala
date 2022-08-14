package compose.lens

import zio.schema.{AccessorBuilder, Schema}

object LambdaAccessor extends AccessorBuilder {
  override type Lens[S, A]      = LambdaLens[S, A]
  override type Prism[S, A]     = Unit
  override type Traversal[S, A] = Unit

  override def makeLens[S, A](product: Schema.Record[S], term: Schema.Field[A]): Lens[S, A] =
    new LambdaLens(product = product, term = term)

  override def makePrism[S, A](sum: Schema.Enum[S], term: Schema.Case[A, S]): Prism[S, A]                    = ()
  override def makeTraversal[S, A](collection: Schema.Collection[S, A], element: Schema[A]): Traversal[S, A] = ()
}
