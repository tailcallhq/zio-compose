package compose.dsl

import compose.{~>, ExecutionPlan, Lambda}
import compose.Lambda.unsafeMake
import zio.schema.Schema

trait TupleDSL[-A, +B] { self: Lambda[A, B] =>
  final def _1[B1, B2](implicit ev: B <:< (B1, B2), s0: Schema[B1], s1: Schema[B2]): A ~> B1 =
    unsafeMake {
      ExecutionPlan.Arg(self.compile, 0, s0.ast, s1.ast)
    }

  final def _2[B1, B2](implicit ev: B <:< (B1, B2), s0: Schema[B1], s1: Schema[B2]): A ~> B2 = unsafeMake {
    ExecutionPlan.Arg(self.compile, 1, s0.ast, s1.ast)
  }
}
