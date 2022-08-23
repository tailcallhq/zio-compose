package compose.dsl
import compose._

trait OptionDSL[-A, +B] { self: A ~> B =>
  import Lambda._
  def isEmpty[C](implicit ev: B <:< Option[C]): A ~> Boolean =
    self.widen >>> make[Option[C], Boolean] { ExecutionPlan.Optional.IsEmpty }

  def nonEmpty[C](implicit ev: B <:< Option[C]): A ~> Boolean = isEmpty.not
}
