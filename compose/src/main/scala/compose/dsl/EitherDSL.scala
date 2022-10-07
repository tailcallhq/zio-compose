package compose.dsl
import compose._

object EitherDSL {
  trait Op[-A, +B] {
    self: A ~> B =>
    def isLeft[L, R](implicit ev: B <:< Either[L, R]): A ~> Boolean = self.widen >>> Lambda.unsafe
      .attempt[Either[L, R], Boolean] { ExecutionPlan.EitherOne.IsLeft }

    def isRight[L, R](implicit ev: B <:< Either[L, R]): A ~> Boolean = isLeft.not
  }
}
