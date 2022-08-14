package compose.dsl

import compose.{~>, Lambda}
import zio.schema.Schema

trait TupleDSL[-A, +B] { self: A ~> B =>
  final def _1[B1, B2](implicit ev: B <:< (B1, B2), s0: Schema[B1], s1: Schema[B2]): A ~> B1 =
    self.asInstanceOf[A ~> (B1, B2)] >>> Lambda._1

  final def _2[B1, B2](implicit ev: B <:< (B1, B2), s0: Schema[B1], s1: Schema[B2]): A ~> B2 =
    self.asInstanceOf[A ~> (B1, B2)] >>> Lambda._2
}
