package compose.dsl

import compose.~>
import compose.ExecutionPlan.Tupled
import compose.Lambda.make

trait TupleDSL[-A, +B] { self: A ~> B =>
  final def _1[B1, B2](implicit ev: B <:< (B1, B2)): A ~> B1 = make[A, B1] {
    Tupled.Arg(self.compile, 0)
  }
  final def _2[B1, B2](implicit ev: B <:< (B1, B2)): A ~> B2 = make[A, B2] {
    Tupled.Arg(self.compile, 1)
  }
}
