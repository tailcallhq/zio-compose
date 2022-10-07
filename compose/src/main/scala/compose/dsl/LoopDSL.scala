package compose.dsl
import compose.ExecutionPlan.Recursive
import compose._

object LoopDSL {
  trait Op[-A, +B] {
    self: A ~> B =>

    final def recurseUntil[B1 >: B <: A](cond: B1 ~> Boolean): B1 ~> B1 = recurseWhile(cond.not)

    final def recurseWhile[B1 >: B <: A](cond: B1 ~> Boolean): B1 ~> B1 = Lambda.unsafe
      .attempt[B1, B1] { Recursive.RecurseWhile(self.compile, cond.compile) }

    final def repeatWhile[B1 >: B](cond: B1 ~> Boolean): A ~> B1 = Lambda.unsafe
      .attempt[A, B1](Recursive.RepeatWhile(self.compile, cond.compile))

    final def repeatUntil[B1 >: B](cond: B1 ~> Boolean): A ~> B1 = repeatWhile(cond.not)
  }
}
