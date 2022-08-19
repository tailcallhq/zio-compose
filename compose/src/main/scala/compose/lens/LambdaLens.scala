package compose.lens

import compose.ExecutionPlan.OpticalExecution
import compose.operation.OpticalOp
import compose.~>
import compose.Lambda.make
import zio.schema.Schema
import zio.prelude.NonEmptyList

final class LambdaLens[S, A](product: Schema.Record[S], term: Schema.Field[A]) {
  def get: S ~> A = make[S, A] {
    OpticalExecution(OpticalOp.GetPath(NonEmptyList(term.label)))
  }

  def set: (S, A) ~> S = make[(S, A), S] {
    OpticalExecution(OpticalOp.SetPath(NonEmptyList(term.label)))
  }
}
