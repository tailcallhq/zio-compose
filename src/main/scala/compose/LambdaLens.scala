package compose

import zio.schema.Schema
import zio.prelude.NonEmptyList

final class LambdaLens[S, A](product: Schema.Record[S], term: Schema.Field[A]) {
  def get: S ~> A      = ExecutionPlan.GetPath(NonEmptyList(term.label)).decompile
  def set: (S, A) ~> S = ExecutionPlan.SetPath(NonEmptyList(term.label)).decompile
}
