package compose.dsl

import compose.~>
import compose.ExecutionPlan.Textual
import compose.Lambda.make

trait StringDSL[-A, +B] { self: A ~> B =>
  final def ++[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit ev: B1 <:< String): A1 ~> B1 =
    self >>> make[B1, B1] { Textual.Concat(other.compile) }

  final def contains[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    self >>> make[B, Boolean](Textual.Contains(other.compile))

  final def endsWith[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    self >>> make[B, Boolean](Textual.EndsWith(other.compile))

  final def length(implicit ev: B <:< String): A ~> Int =
    self >>> make[B, Int](Textual.Length)

  final def lowerCase(implicit ev: B <:< String): A ~> String =
    self >>> make[B, String](Textual.LowerCase)

  final def startsWith[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
    self >>> make[B, Boolean](Textual.StartsWith(other.compile))

  final def upperCase(implicit ev: B <:< String): A ~> String =
    self >>> make[B, String](Textual.UpperCase)
}
