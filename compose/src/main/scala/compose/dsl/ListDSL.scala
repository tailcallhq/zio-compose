package compose.dsl

import compose.{ExecutionPlan, Lambda, ~>}
import zio.schema.Schema

object ListDSL {
  trait Op[-A, +B] {
    self: A ~> B =>
    def find[T](f: T ~> Boolean)(implicit schema: Schema[T], ev: B <:< List[T]): A ~> Option[T] =
      self.widen >>> Lambda.unsafe.attempt[List[T], Option[T]] {
        ExecutionPlan.ListLike.Find(schema.ast, f.compile)
      }
  }
}
