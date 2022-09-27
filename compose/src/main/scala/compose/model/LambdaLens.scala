package compose.model

import compose.ExecutionPlan.Optical
import compose.{Lambda, ~>}
import zio.prelude.NonEmptyList
import zio.schema.Schema

final class LambdaLens[S, A](term: Schema.Field[A]) {
  def get: S ~> A = Lambda.unsafe.attempt[S, A] {
    Optical.GetPath(NonEmptyList(term.label))
  }

  def set: (S, A) ~> S = Lambda.unsafe.attempt[(S, A), S] {
    Optical.SetPath(NonEmptyList(term.label))
  }
}
