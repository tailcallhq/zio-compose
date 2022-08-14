package compose.dsl

import compose.{~>, ExecutionPlan}
import compose.Lambda.{constant, unsafeMake}

trait BooleanDSL[-A, +B] { self: A ~> B =>
  final def &&[A1](other: A1 ~> Boolean)(implicit ev: B <:< Boolean): A1 ~> Boolean =
    self and other

  final def ||[A1](other: A1 ~> Boolean)(implicit ev: B <:< Boolean): A1 ~> Boolean =
    self or other

  final def and[A1](other: A1 ~> Boolean)(implicit ev: B <:< Boolean): A1 ~> Boolean =
    unsafeMake {
      ExecutionPlan.LogicalAnd(self.compile, other.compile)
    }

  final def diverge[C](isTrue: B ~> C, isFalse: B ~> C)(implicit ev: B <:< Boolean): A ~> C =
    unsafeMake {
      ExecutionPlan.IfElse(self.compile, isTrue.compile, isFalse.compile)
    }

  final def isFalse(implicit ev: B <:< Boolean): A ~> Boolean =
    self =:= constant(false)

  final def isTrue(implicit ev: B <:< Boolean): A ~> Boolean =
    self =:= constant(true)

  final def not(implicit ev: B <:< Boolean): A ~> Boolean = unsafeMake {
    ExecutionPlan.LogicalNot(self.compile)
  }

  final def or[A1](other: A1 ~> Boolean)(implicit ev: B <:< Boolean): A1 ~> Boolean =
    unsafeMake {
      ExecutionPlan.LogicalOr(self.compile, other.compile)
    }

}
