package compose.dsl

import compose.~>
import compose.ExecutionPlan.Textual
import compose.Lambda.make

trait StringDSL[-A, +B] { self: A ~> B =>
  final def ++[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit ev: B1 <:< String): A1 ~> B1 =
    make[A, B] { Textual.Concat(self.compile, other.compile) }

  final def contains[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    make[A1, Boolean](Textual.Contains(self.compile, other.compile))

  final def endsWith[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    make[A1, Boolean](Textual.EndsWith(self.compile, other.compile))

  final def length(implicit ev: B <:< String): A ~> Int =
    make[A, Int](Textual.Length(self.compile))

  final def lowerCase(implicit ev: B <:< String): A ~> String =
    make[A, String](Textual.LowerCase(self.compile))

  final def startsWith[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    make[A1, Boolean](Textual.StartsWith(self.compile, other.compile))

  final def upperCase(implicit ev: B <:< String): A ~> String =
    make[A, String](Textual.UpperCase(self.compile))
}
