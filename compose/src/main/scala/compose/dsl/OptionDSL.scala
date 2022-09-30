package compose.dsl
import compose._

object OptionDSL {
  trait Op[-A, +B] {
    self: A ~> B =>
    def isEmpty[C](implicit ev: B <:< Option[C]): A ~> Boolean =
      self.widen >>> Lambda.unsafe.attempt[Option[C], Boolean] {
        ExecutionPlan.Optional.IsEmpty
      }

    def nonEmpty[C](implicit ev: B <:< Option[C]): A ~> Boolean = isEmpty.not
  }
}
