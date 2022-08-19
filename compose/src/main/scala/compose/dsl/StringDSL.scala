package compose.dsl

import compose.~>
import compose.execution.ExecutionPlan.StringOperation
import compose.Lambda.make

trait StringDSL[-A, +B] { self: A ~> B =>
  final def contains[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    make[A1, Boolean](StringOperation.Contains(self.compile, other.compile))

  final def endsWith[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    make[A1, Boolean](StringOperation.EndsWith(self.compile, other.compile))

  final def length(implicit ev: B <:< String): A ~> Int =
    make[A, Int](StringOperation.Length(self.compile))

  final def lowerCase(implicit ev: B <:< String): A ~> String =
    make[A, String](StringOperation.LowerCase(self.compile))

  final def startsWith[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    make[A1, Boolean](StringOperation.StartsWith(self.compile, other.compile))

  final def upperCase(implicit ev: B <:< String): A ~> String =
    make[A, String](StringOperation.UpperCase(self.compile))
}
