package compose.dsl
import compose._
import compose.ExecutionPlan.Recursive

trait LoopDSL[-A, +B] { self: A ~> B =>
  import Lambda._

  final def doWhile(cond: Any ~> Boolean): A ~> B =
    make[A, B](Recursive.DoWhile(self.compile, cond.compile))

  final def doUntil(cond: Any ~> Boolean): A ~> B =
    doWhile(cond.not)

  final def recurseUntil[B1 >: B <: A](cond: B1 ~> Boolean): B1 ~> B1 =
    recurseWhile(cond.not)

  final def recurseWhile[B1 >: B <: A](cond: B1 ~> Boolean): B1 ~> B1 =
    make[B1, B1] { Recursive.RecurseWhile(self.compile, cond.compile) }
}
