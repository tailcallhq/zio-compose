package compose.dsl

import compose.{~>, ExecutionPlan, IsNumeric, Lambda, Numeric}
import compose.Lambda.{constant, unsafeMake}
import zio.schema.Schema

trait NumericDSL[-A, +B] { self: A ~> B =>
  final def >[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> Boolean =
    self gt other

  final def >=[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> Boolean =
    self gte other

  final def <[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> Boolean =
    self lt other

  final def <=[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> Boolean =
    self lte other

  final def -[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1], s: Schema[B1]): A1 ~> B1 =
    numOp(Numeric.Operation.Add, other.negate)

  final def +[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> B1 =
    numOp(Numeric.Operation.Add, other)

  final def *[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> B1 =
    numOp(Numeric.Operation.Multiply, other)

  final def dec[B1 >: B](implicit num: IsNumeric[B1], s: Schema[B1]): A ~> B1 = self - Lambda.constant(num.one)

  final def gt[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> Boolean =
    numOp(Numeric.Operation.GreaterThan, other) =:= constant(1)

  final def gte[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> Boolean =
    numOp(Numeric.Operation.GreaterThanEqualTo, other) =:= constant(1)

  final def inc[B1 >: B](implicit num: IsNumeric[B1], s: Schema[B1]): A ~> B1 = self + Lambda.constant(num.one)

  final def lt[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> Boolean =
    (self >= other).not

  final def lte[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> Boolean =
    (self gt other).not

  final def negate[B1 >: B](implicit num: IsNumeric[B1], schema: Schema[B1]): A ~> B1 =
    self * Lambda.constant(num.negativeOne)

  private final def numOp[A1 <: A, B1 >: B](operation: Numeric.Operation, other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 =
    unsafeMake {
      ExecutionPlan.NumericOperation(operation, self.compile, other.compile, num.toDynamic)
    }

}
