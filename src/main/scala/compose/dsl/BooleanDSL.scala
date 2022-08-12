package compose.dsl

import compose.{~>, ExecutionPlan}
import compose.Lambda.{constant, unsafeMake}

trait BooleanDSL[-A, +B] { self: A ~> B =>
  def &&[A1](other: A1 ~> Boolean)(implicit ev: B <:< Boolean): A1 ~> Boolean =
    self and other

  def ||[A1](other: A1 ~> Boolean)(implicit ev: B <:< Boolean): A1 ~> Boolean =
    self or other

  def and[A1](other: A1 ~> Boolean)(implicit ev: B <:< Boolean): A1 ~> Boolean =
    unsafeMake {
      ExecutionPlan.LogicalAnd(self.compile, other.compile)
    }

  def isTrue(implicit ev: B <:< Boolean): A ~> Boolean =
    self =:= constant(true)

  def isFalse(implicit ev: B <:< Boolean): A ~> Boolean =
    self =:= constant(false)

  def not(implicit ev: B <:< Boolean): A ~> Boolean = unsafeMake {
    ExecutionPlan.LogicalNot(self.compile)
  }

  def or[A1](other: A1 ~> Boolean)(implicit ev: B <:< Boolean): A1 ~> Boolean =
    unsafeMake {
      ExecutionPlan.LogicalOr(self.compile, other.compile)
    }
}
