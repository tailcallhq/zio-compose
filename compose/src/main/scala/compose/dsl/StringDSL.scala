package compose.dsl

import compose.~>
import compose.execution.ExecutionPlan.StringOperation
import compose.Lambda.make

trait StringDSL[-A, +B] { self: A ~> B =>
  final def ++[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit ev: B1 <:< String): A1 ~> B1 =
    make[A, B] { StringOperation(StringOperation.Concat(self.compile, other.compile)) }

  final def contains[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    make[A1, Boolean](StringOperation(StringOperation.Contains(self.compile, other.compile)))

  final def endsWith[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    make[A1, Boolean](StringOperation(StringOperation.EndsWith(self.compile, other.compile)))

  final def length(implicit ev: B <:< String): A ~> Int =
    make[A, Int](StringOperation(StringOperation.Length(self.compile)))

  final def lowerCase(implicit ev: B <:< String): A ~> String =
    make[A, String](StringOperation(StringOperation.LowerCase(self.compile)))

  final def startsWith[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    make[A1, Boolean](StringOperation(StringOperation.StartsWith(self.compile, other.compile)))

  final def upperCase(implicit ev: B <:< String): A ~> String =
    make[A, String](StringOperation(StringOperation.UpperCase(self.compile)))
}
