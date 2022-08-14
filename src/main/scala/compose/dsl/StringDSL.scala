package compose.dsl

import compose.{~>, Lambda}
import compose.ExecutionPlan.StringOperation

trait StringDSL[-A, +B] { self: A ~> B =>
  final def length(implicit ev: B <:< String): A ~> Int = stringOp(StringOperation.Length)

  final def lowerCase(implicit ev: B <:< String): A ~> String = stringOp(StringOperation.LowerCase)

  final def upperCase(implicit ev: B <:< String): A ~> String = stringOp(StringOperation.UpperCase)

  private final def stringOp(operation: StringOperation): A ~> Nothing = self >>> Lambda.unsafeMake { operation }
}
