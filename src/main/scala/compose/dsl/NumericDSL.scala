package compose.dsl

import compose.{~>, ExecutionPlan, IsNumeric, Lambda, Numeric}
import zio.schema.Schema

trait NumericDSL[-A, +B] { self: Lambda[A, B] =>
  final def >[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> Boolean = numOp(Numeric.Operation.GreaterThan, other) === Lambda.constant(1)

  final def >=[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> Boolean = numOp(Numeric.Operation.GreaterThanEqualTo, other) === Lambda.constant(1)

  final def +[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 = numOp(Numeric.Operation.Add, other)

  final def gt[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 = numOp(Numeric.Operation.GreaterThan, other)

  final def gte[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 = numOp(Numeric.Operation.GreaterThanEqualTo, other)

  final def inc[B1 >: B](implicit num: IsNumeric[B1], s: Schema[B1]): A ~> B1 = self + Lambda.constant(num.one)

  final def negate[B1 >: B](implicit
    num: IsNumeric[B1],
    schema: Schema[B1],
  ): A ~> B1 = self * Lambda.constant(num.negativeOne)

  private final def numOp[A1 <: A, B1 >: B](operation: Numeric.Operation, other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 =
    ExecutionPlan.NumericOperation(operation, self.compile, other.compile, num.toDynamic).decompile

  final def *[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 = numOp(Numeric.Operation.Multiply, other)

}
