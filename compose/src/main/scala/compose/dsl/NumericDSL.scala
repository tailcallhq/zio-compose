package compose.dsl

import compose.{~>, ExecutionPlan, Lambda}
import compose.Lambda.{constant, make}
import compose.dsl.NumericDSL.IsNumeric
import zio.schema.{DeriveSchema, DynamicValue, Schema}

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
    numOp(NumericDSL.Operation.Add, other.negate)

  final def +[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> B1 =
    numOp(NumericDSL.Operation.Add, other)

  final def *[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> B1 =
    numOp(NumericDSL.Operation.Multiply, other)

  final def dec[B1 >: B](implicit num: IsNumeric[B1], s: Schema[B1]): A ~> B1 = self - Lambda.constant(num.one)

  final def gt[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> Boolean =
    numOp(NumericDSL.Operation.GreaterThan, other) =:= constant(1)

  final def gte[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> Boolean =
    numOp(NumericDSL.Operation.GreaterThanEqualTo, other) =:= constant(1)

  final def inc[B1 >: B](implicit num: IsNumeric[B1], s: Schema[B1]): A ~> B1 = self + Lambda.constant(num.one)

  final def lt[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> Boolean =
    (self >= other).not

  final def lte[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit num: IsNumeric[B1]): A1 ~> Boolean =
    (self gt other).not

  final def negate[B1 >: B](implicit num: IsNumeric[B1], schema: Schema[B1]): A ~> B1 =
    self * Lambda.constant(num.negativeOne)

  private final def numOp[A1 <: A, B1 >: B](operation: NumericDSL.Operation, other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 =
    make[A1, B1] {
      ExecutionPlan.NumericOperation(operation, self.compile, other.compile, num.toDynamic)
    }

}

object NumericDSL {
  sealed trait Operation

  object Operation {
    case object Add                extends Operation
    case object Multiply           extends Operation
    case object Subtract           extends Operation
    case object Divide             extends Operation
    case object GreaterThan        extends Operation
    case object GreaterThanEqualTo extends Operation
  }

  sealed trait IsNumeric[A] {
    self =>
    def one: A

    def negativeOne: A

    def toDynamic: DynamicValue = IsNumeric.schema.toDynamic(self)
  }

  object IsNumeric {
    implicit case object NumericInt extends IsNumeric[Int] {
      override def one: Int = 1

      override def negativeOne: Int = -1
    }

    implicit def schema: Schema[IsNumeric[_]] = DeriveSchema.gen[IsNumeric[_]]
  }

}
