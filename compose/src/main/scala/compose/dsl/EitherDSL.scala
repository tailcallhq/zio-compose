package compose.dsl
import compose._

trait EitherDSL[-A, +B] { self: A ~> B =>
  import Lambda._
  def isLeft[L, R](implicit ev: B <:< Either[L, R]): A ~> Boolean =
    self.widen >>> make[Either[L, R], Boolean] { ExecutionPlan.EitherOne.IsLeft }

  def isRight[L, R](implicit ev: B <:< Either[L, R]): A ~> Boolean =
    isLeft.not
}
