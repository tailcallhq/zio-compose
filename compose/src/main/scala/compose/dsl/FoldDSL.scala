package compose.dsl

import compose.{ExecutionPlan, Lambda, ~>}
import zio.schema.Schema

object FoldDSL {
  trait Op[-A, +B] {
    self: A ~> B =>
    def fold[L, R, C](l: L ~> C)(r: R ~> C)(implicit ev: Fold[B, L, R, C]): A ~> C = self >>> ev
      .fold(l, r)
  }

  sealed trait Fold[-A, +L, +R, -B] {
    def fold[B1 <: B](l: L ~> B1, r: R ~> B1): A ~> B1
  }

  trait Implicits {
    implicit def foldOption[B, C]: Fold[Option[B], Any, B, C] = new Fold[Option[B], Any, B, C] {
      def fold[C1 <: C](l: Any ~> C1, r: B ~> C1): Option[B] ~> C1 = Lambda.unsafe
        .attempt[Option[B], C1] { ExecutionPlan.Fold.FoldOption(l.compile, r.compile) }
    }

    implicit def foldEither[L, R, C]: Fold[Either[L, R], L, R, C] =
      new Fold[Either[L, R], L, R, C] {
        def fold[C1 <: C](l: L ~> C1, r: R ~> C1): Either[L, R] ~> C1 = Lambda.unsafe
          .attempt[Either[L, R], C1] { ExecutionPlan.Fold.FoldEither(l.compile, r.compile) }
      }

    implicit def foldSeq[T, S](implicit schema: Schema[T]): Fold[Seq[T], S, (S, T), S] =
      new Fold[Seq[T], S, (S, T), S] {
        override def fold[B1 <: S](l: S ~> B1, r: (S, T) ~> B1): Seq[T] ~> B1 = Lambda.unsafe
          .attempt[Seq[T], B1] { ExecutionPlan.Fold.FoldList(schema.ast, l.compile, r.compile) }
      }
  }

}
