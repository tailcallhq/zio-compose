package compose.dsl

import compose.{~>, Lambda}
import compose.ExecutionPlan.StringOperation

trait StringDSL[-A, +B] { self: A ~> B =>
  final def contains[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    stringOp[Boolean](StringOperation.Contains(other.compile))

  final def endsWith[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    stringOp[Boolean](StringOperation.EndsWith(other.compile))

  final def length(implicit ev: B <:< String): A ~> Int = stringOp[Int](StringOperation.Length)

  final def lowerCase(implicit ev: B <:< String): A ~> String = stringOp(StringOperation.LowerCase)

  final def startsWith[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    stringOp(StringOperation.StartsWith(other.compile))

  final def upperCase(implicit ev: B <:< String): A ~> String = stringOp(StringOperation.UpperCase)

  private final def stringOp[C](operation: StringOperation): A ~> C =
    self >>> Lambda.make[B, C] { operation }
}
