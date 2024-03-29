package compose.dsl

import compose.ExecutionPlan.Textual
import compose.{Lambda, ~>}

object StringDSL {
  trait Op[-A, +B] {
    self: A ~> B =>
    final def ++[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit ev: B1 <:< String): A1 ~> B1 = Lambda
      .unsafe.attempt[A, B] { Textual.Concat(self.compile, other.compile) }

    final def contains[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
      Lambda.unsafe.attempt[A1, Boolean](Textual.Contains(self.compile, other.compile))

    final def endsWith[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
      Lambda.unsafe.attempt[A1, Boolean](Textual.EndsWith(self.compile, other.compile))

    final def length(implicit ev: B <:< String): A ~> Int = Lambda.unsafe
      .attempt[A, Int](Textual.Length(self.compile))

    final def lowerCase(implicit ev: B <:< String): A ~> String = Lambda.unsafe
      .attempt[A, String](Textual.LowerCase(self.compile))

    final def startsWith[A1 <: A](other: A1 ~> String)(implicit ev: B <:< String): A1 ~> Boolean =
      Lambda.unsafe.attempt[A1, Boolean](Textual.StartsWith(self.compile, other.compile))

    final def upperCase(implicit ev: B <:< String): A ~> String = Lambda.unsafe
      .attempt[A, String](Textual.UpperCase(self.compile))
  }

  import Lambda._

  trait Implicits {
    implicit class ComposeStringInterpolator(val sc: StringContext) {
      def cs[A](args: (A ~> String)*): A ~> String = {
        val strings          = sc.parts.iterator
        val lambdas          = args.iterator
        var buf: A ~> String = constant(strings.next())
        while (strings.hasNext) { buf = buf ++ lambdas.next() ++ constant(strings.next()) }
        buf
      }
    }
  }

}
