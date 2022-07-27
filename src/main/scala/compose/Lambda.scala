package compose

import zio.prelude.NonEmptyList
import zio.schema.Schema
import zio.Task

sealed trait Lambda[-A, +B] { self =>
  def ===[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean =
    Lambda.Equals(self, other)

  final def >[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> Boolean = numOp(Numeric.Operation.GreaterThan, other) === Lambda.constant(1)

  final def >=[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> Boolean = numOp(Numeric.Operation.GreaterThanEqualTo, other) === Lambda.constant(1)

  final def >>>[C](other: Lambda[B, C]): Lambda[A, C] = self pipe other

  final def <<<[X](other: Lambda[X, A]): Lambda[X, B] = self compose other

  final def <*>[A1 <: A, B1 >: B, B2](
    other: Lambda[A1, B2],
  )(implicit
    b1: Schema[B1],
    b2: Schema[B2],
  ): A1 ~> (B1, B2) =
    (self: A1 ~> B1) zip other

  final def +[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 = numOp(Numeric.Operation.Add, other)

  final def *[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 = numOp(Numeric.Operation.Multiply, other)

  final def apply[A1 <: A, B1 >: B](a: A1)(implicit in: Schema[A1], out: Schema[B1]): Task[B1] =
    Interpreter.evalTyped[B1](ExecutionPlan.fromLambda(self), in.toDynamic(a))

  final def compose[X](other: Lambda[X, A]): Lambda[X, B] =
    Lambda.Pipe(other, self)

  final def gt[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 = numOp(Numeric.Operation.GreaterThan, other)

  final def gte[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 = numOp(Numeric.Operation.GreaterThanEqualTo, other)

  final def negate[B1 >: B](implicit
    num: IsNumeric[B1],
    schema: Schema[B1],
  ): A ~> B1 = Lambda.constant(num.one) * self

  final def pipe[C](other: Lambda[B, C]): Lambda[A, C] =
    Lambda.Pipe(self, other)

  final def zip[A1 <: A, B1 >: B, B2](other: Lambda[A1, B2])(implicit
    b1: Schema[B1],
    b2: Schema[B2],
  ): A1 ~> (B1, B2) = Lambda.Combine(self, other, b1, b2)

  private[compose] final def compile: ExecutionPlan =
    ExecutionPlan.fromLambda(self)

  private final def numOp[A1 <: A, B1 >: B](operation: Numeric.Operation, other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 =
    Lambda.NumericOperation(operation, self, other, num)
}

object Lambda {

  def constant[B](a: B)(implicit schema: Schema[B]): Any ~> B =
    Constant(a, schema)

  def default[A](implicit schema: Schema[A]): Any ~> A = Default(schema)

  def fromMap[A, B](
    source: Map[A, B],
  )(implicit input: Schema[A], output: Schema[B]): Lambda[A, B] =
    FromMap(input, source, output)

  def identity[A]: Lambda[A, A] = Identity[A]()

  def ifElse[A, B](f: A ~> Boolean)(isTrue: A ~> B, isFalse: A ~> B): A ~> B =
    IfElse(f, isTrue, isFalse)

  def transform[A, B](transformations: Transform[A, B]*)(implicit b: Schema[B]): A ~> B =
    Transformation(transformations.toList, b)

  final case class Default[A](value: Schema[A]) extends Lambda[Any, A]

  final case class Transformation[A, B](value: List[Transform[A, B]], output: Schema[B]) extends Lambda[A, B]

  final case class Equals[A, B](left: A ~> B, right: A ~> B) extends Lambda[A, Boolean]

  final case class FromMap[A, B](input: Schema[A], source: Map[A, B], output: Schema[B]) extends Lambda[A, B]

  final case class Constant[B](b: B, schema: Schema[B]) extends Lambda[Any, B]

  final case class Identity[A]() extends Lambda[A, A]

  final case class Pipe[A, B, C](f: Lambda[A, B], g: Lambda[B, C]) extends Lambda[A, C]

  final case class GetPath[A, B](input: Schema[A], path: NonEmptyList[String], output: Schema[B]) extends Lambda[A, B]

  final case class SetPath[A, B](whole: Schema[A], path: NonEmptyList[String], piece: Schema[B])
      extends Lambda[(A, B), A]

  final case class IfElse[A, B](f: A ~> Boolean, ifTrue: A ~> B, ifFalse: A ~> B) extends Lambda[A, B]

  final case class Combine[A, B1, B2](left: A ~> B1, right: A ~> B2, o1: Schema[B1], o2: Schema[B2])
      extends Lambda[A, (B1, B2)]

  case class NumericOperation[A, B](
    operation: Numeric.Operation,
    left: A ~> B,
    right: A ~> B,
    num: IsNumeric[B],
  ) extends Lambda[A, B]

  case class LogicalAnd[A](left: A ~> Boolean, right: A ~> Boolean) extends Lambda[A, Boolean]

  case class LogicalOr[A](left: A ~> Boolean, right: A ~> Boolean) extends Lambda[A, Boolean]

  case class LogicalNot[A](logic: A ~> Boolean) extends Lambda[A, Boolean]
}
