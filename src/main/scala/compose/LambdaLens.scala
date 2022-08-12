package compose

import compose.Lambda.unsafeMake
import zio.schema.Schema
import zio.prelude.NonEmptyList

final class LambdaLens[S, A](product: Schema.Record[S], term: Schema.Field[A]) {
  def get: S ~> A = unsafeMake {
    ExecutionPlan.GetPath(NonEmptyList(term.label))
  }

  def set: (S, A) ~> S = unsafeMake {
    ExecutionPlan.SetPath(NonEmptyList(term.label))
  }
}
