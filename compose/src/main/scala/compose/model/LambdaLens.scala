package compose.model

import compose.ExecutionPlan.Optical
import compose.~>
import compose.Lambda.attempt
import zio.schema.Schema
import zio.prelude.NonEmptyList

final class LambdaLens[S, A](term: Schema.Field[A]) {
  def get: S ~> A = attempt[S, A] {
    Optical.GetPath(NonEmptyList(term.label))
  }

  def set: (S, A) ~> S = attempt[(S, A), S] {
    Optical.SetPath(NonEmptyList(term.label))
  }
}
