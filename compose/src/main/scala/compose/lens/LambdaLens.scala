package compose.lens

import compose.~>
import compose.Lambda.make
import compose.execution.ExecutionPlan.OpticalOperation
import zio.schema.Schema
import zio.prelude.NonEmptyList

final class LambdaLens[S, A](product: Schema.Record[S], term: Schema.Field[A]) {
  def get: S ~> A = make[S, A] {
    OpticalOperation(OpticalOperation.GetPath(NonEmptyList(term.label)))
  }

  def set: (S, A) ~> S = make[(S, A), S] {
    OpticalOperation(OpticalOperation.SetPath(NonEmptyList(term.label)))
  }
}
