package compose

import zio.schema.Schema
import zio.prelude.NonEmptyList

final class LambdaLens[S, A](product: Schema.Record[S], term: Schema.Field[A]) {
  def get: S ~> A      = Lambda.GetPath(product, NonEmptyList(term.label), term.schema)
  def set: (S, A) ~> S = Lambda.SetPath(product, NonEmptyList(term.label), term.schema)
}
