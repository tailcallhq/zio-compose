package compose.dsl
import compose._
import compose.ExecutionPlan.Recursive

trait LoopDSL[-A, +B] { self: A ~> B =>
  import Lambda._

  final def recurseUntil[B1 >: B <: A](cond: B1 ~> Boolean): B1 ~> B1 =
    recurseWhile(cond.not)

  final def recurseWhile[B1 >: B <: A](cond: B1 ~> Boolean): B1 ~> B1 =
    make[B1, B1] { Recursive.RecurseWhile(self.compile, cond.compile) }

  final def repeatWhile[B1 >: B](cond: B1 ~> Boolean): A ~> B1 =
    make[A, B1](Recursive.RepeatWhile(self.compile, cond.compile))

  final def repeatUntil[B1 >: B](cond: B1 ~> Boolean): A ~> B1 =
    repeatWhile(cond.not)
}
