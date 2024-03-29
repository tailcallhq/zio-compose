package compose.dsl

import compose.ExecutionPlan.Logical
import compose.Lambda.constant
import compose.{Lambda, ~>}

object BooleanDSL {
  trait Op[-A, +B] {
    self: A ~> B =>
    final def &&[A1](other: A1 ~> Boolean)(implicit ev: B <:< Boolean): A1 ~> Boolean =
      self and other

    final def ||[A1](other: A1 ~> Boolean)(implicit ev: B <:< Boolean): A1 ~> Boolean =
      self or other

    final def and[A1](other: A1 ~> Boolean)(implicit ev: B <:< Boolean): A1 ~> Boolean = Lambda
      .unsafe.attempt[A1, Boolean] { Logical.And(self.compile, other.compile) }

    final def diverge[A1 <: A, C](isTrue: A1 ~> C, isFalse: A1 ~> C)(implicit
      ev: B <:< Boolean,
    ): A ~> C = Lambda.unsafe
      .attempt[A, C] { Logical.Diverge(self.compile, isTrue.compile, isFalse.compile) }

    final def eq[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean = Lambda.unsafe
      .attempt[A1, Boolean] { Logical.Equals(self.compile, other.compile) }

    final def isFalse(implicit ev: B <:< Boolean): A ~> Boolean = self.widen =:= constant(false)

    final def isTrue(implicit ev: B <:< Boolean): A ~> Boolean = self.widen =:= constant(true)

    final def not(implicit ev: B <:< Boolean): A ~> Boolean = Lambda.unsafe
      .attempt[A, Boolean] { Logical.Not(self.compile) }

    final def or[A1](other: A1 ~> Boolean)(implicit ev: B <:< Boolean): A1 ~> Boolean = Lambda
      .unsafe.attempt[A1, Boolean] { Logical.Or(self.compile, other.compile) }
  }
}
