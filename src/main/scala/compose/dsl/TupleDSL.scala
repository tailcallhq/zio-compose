package compose.dsl

import compose.{~>, ExecutionPlan, Lambda}
import zio.schema.Schema

trait TupleDSL[-A, +B] { self: Lambda[A, B] =>
  final def _1[B1, B2](implicit ev: B <:< (B1, B2), s0: Schema[B1], s1: Schema[B2]): A ~> B1 =
    ExecutionPlan.Arg(0, s0.ast, s1.ast).decompile

  final def _2[B1, B2](implicit ev: B <:< (B1, B2), s0: Schema[B1], s1: Schema[B2]): A ~> B2 =
    ExecutionPlan.Arg(1, s0.ast, s1.ast).decompile
}
