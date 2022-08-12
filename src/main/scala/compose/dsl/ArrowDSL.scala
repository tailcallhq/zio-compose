package compose.dsl

import compose.{~>, CanConcat, ExecutionPlan, Lambda}
import compose.Lambda.unsafeMake
import zio.schema.Schema

trait ArrowDSL[-A, +B] { self: A ~> B =>
  final def =!=[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean =
    self notEq other

  final def =:=[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean =
    self eq other

  final def >>>[C](other: B ~> C): A ~> C = self pipe other

  final def <<<[X](other: X ~> A): X ~> B = self compose other

  final def <*[A1 <: A, B1 >: B, B2](other: A1 ~> B2)(implicit b1: Schema[B1], b2: Schema[B2]): A1 ~> B1 =
    (self: A1 ~> B1) zipLeft other

  final def <*>[A1 <: A, B1 >: B, B2](other: A1 ~> B2)(implicit b1: Schema[B1], b2: Schema[B2]): A1 ~> (B1, B2) =
    (self: A1 ~> B1) zip other

  final def ++[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit ev: CanConcat[B1]): A1 ~> B1 =
    unsafeMake { ExecutionPlan.Concat(self.compile, other.compile, Schema[CanConcat[_]].toDynamic(ev)) }

  final def *>[A1 <: A, B1 >: B, B2](other: A1 ~> B2)(implicit b1: Schema[B1], b2: Schema[B2]): A1 ~> B2 =
    (self: A1 ~> B1) zipRight other

  final def bind[A1 <: A](a: A1)(implicit ev: Schema[A1]): Any ~> B = Lambda.constant(a) >>> self

  final def compose[X](other: X ~> A): X ~> B =
    other pipe self

  final def diverge[C](isTrue: B ~> C, isFalse: B ~> C)(implicit ev: B <:< Boolean): A ~> C =
    unsafeMake { ExecutionPlan.IfElse(self.compile, isTrue.compile, isFalse.compile) }

  final def eq[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean =
    unsafeMake { ExecutionPlan.Equals(self.compile, other.compile) }

  final def notEq[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean =
    (self =:= other).not

  final def pipe[C](other: B ~> C): A ~> C =
    unsafeMake { ExecutionPlan.Pipe(self.compile, other.compile) }

  final def zip[A1 <: A, B1 >: B, B2](other: A1 ~> B2): A1 ~> (B1, B2) =
    unsafeMake { ExecutionPlan.Zip(self.compile, other.compile) }

  final def zipLeft[A1 <: A, B1 >: B, B2](other: A1 ~> B2)(implicit b1: Schema[B1], b2: Schema[B2]): A1 ~> B1 =
    ((self: A1 ~> B1) <*> other)._1

  final def zipRight[A1 <: A, B1 >: B, B2](other: A1 ~> B2)(implicit b1: Schema[B1], b2: Schema[B2]): A1 ~> B2 =
    ((self: A1 ~> B1) <*> other)._2
}
