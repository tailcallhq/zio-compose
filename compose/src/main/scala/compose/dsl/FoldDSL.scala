package compose.dsl

import compose.{ExecutionPlan, Lambda, ~>}

import scala.util.Either

object FoldDSL {
  trait Op[-A, +B] {
    self: A ~> B =>
    def fold[L, R, C](l: L ~> C, r: R ~> C)(implicit ev: Fold[B, L, R, C]): A ~> C = self >>> ev.fold(l, r)
  }

  sealed trait Fold[-B, +L, +R, -C] {
    def fold[C1 <: C](l: L ~> C1, r: R ~> C1): B ~> C1
  }

  trait Implicits {
    implicit def foldOption[B, C]: Fold[Option[B], Any, B, C] = new Fold[Option[B], Any, B, C] {
      def fold[C1 <: C](l: Any ~> C1, r: B ~> C1): Option[B] ~> C1 =
        Lambda.unsafe.attempt[Option[B], C1] {
          ExecutionPlan.Fold.FoldOption(l.compile, r.compile)
        }
    }

    implicit def foldEither[L, R, C]: Fold[Either[L, R], L, R, C] = new Fold[Either[L, R], L, R, C] {
      def fold[C1 <: C](l: L ~> C1, r: R ~> C1): Either[L, R] ~> C1 =
        Lambda.unsafe.attempt[Either[L, R], C1] {
          ExecutionPlan.Fold.FoldEither(l.compile, r.compile)
        }
    }
  }

}
