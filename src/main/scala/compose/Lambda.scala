package compose

import zio.prelude.NonEmptyList
import zio.schema.Schema
import zio.Task

sealed trait Lambda[-A, +B] { self =>
  final def ===[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean =
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

  final def *>[A1 <: A, B1 >: B, B2](
    other: Lambda[A1, B2],
  )(implicit
    b1: Schema[B1],
    b2: Schema[B2],
  ): A1 ~> B2 =
    ((self: A1 ~> B1) <*> other) >>> Lambda.arg1

  final def ->>[I >: B, C](other: (C, I) ~> C)(implicit i: Schema[I]): Transformation[A, C] =
    self transform other

  final def +[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 = numOp(Numeric.Operation.Add, other)

  final def *[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 = numOp(Numeric.Operation.Multiply, other)

  final def ++[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit ev: CanConcat[B1]): A1 ~> B1 = Lambda.Concat(self, other, ev)

  final def apply[A1 <: A, B1 >: B](a: A1)(implicit in: Schema[A1], out: Schema[B1]): Task[B1] =
    Interpreter.evalTyped[B1](ExecutionPlan.from(self), in.toDynamic(a))

  final def compose[X](other: Lambda[X, A]): Lambda[X, B] =
    Lambda.Pipe(other, self)

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
  ): A ~> B1 = Lambda.constant(num.one) * self

  final def pipe[C](other: Lambda[B, C]): Lambda[A, C] =
    Lambda.Pipe(self, other)

  final def repeatUntil[A1 <: A, B1 >: B, X](cond: X ~> Boolean)(implicit a: A1 =:= X, b: B1 =:= X): X ~> X =
    Lambda.RepeatUntil(self.asInstanceOf[X ~> X], cond)

  final def transform[I >: B, C](other: (C, I) ~> C)(implicit i: Schema[I]): Transformation[A, C] =
    Transformation[A, C, I](self, other)

  final def zip[A1 <: A, B1 >: B, B2](other: Lambda[A1, B2])(implicit
    b1: Schema[B1],
    b2: Schema[B2],
  ): A1 ~> (B1, B2) = Lambda.Combine(self, other, b1, b2)

  private[compose] final def compile: ExecutionPlan =
    ExecutionPlan.from(self)

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

  def transform[A, B](transformations: Transformation[A, B]*)(implicit b: Schema[B]): A ~> B =
    Transform(transformations.toList, b)

  def seq[A](f: A ~> Unit*): A ~> Unit = f.reduceLeft(_ *> _)

  def scope[A, B](f: ScopeContext => A ~> B): A ~> B = f(new ScopeContext {})

  def arg0[A0, A1](implicit s0: Schema[A0], s1: Schema[A1]): (A0, A1) ~> A0 = Arg0(s0, s1)

  def arg1[A0, A1](implicit s0: Schema[A0], s1: Schema[A1]): (A0, A1) ~> A1 = Arg1(s0, s1)

  case class Arg0[A0, A1](s0: Schema[A0], s1: Schema[A1]) extends Lambda[(A0, A1), A0]

  case class Arg1[A0, A1](s0: Schema[A0], s1: Schema[A1]) extends Lambda[(A0, A1), A1]

  final case class RepeatUntil[A](f: A ~> A, cond: A ~> Boolean) extends Lambda[A, A]

  final case class Concat[A, B](self: A ~> B, other: A ~> B, canConcat: CanConcat[B]) extends Lambda[A, B]

  final case class Default[A](value: Schema[A]) extends Lambda[Any, A]

  final case class Transform[A, B](value: List[Transformation[A, B]], output: Schema[B]) extends Lambda[A, B]

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

  final case class NumericOperation[A, B](
    operation: Numeric.Operation,
    left: A ~> B,
    right: A ~> B,
    num: IsNumeric[B],
  ) extends Lambda[A, B]

  final case class LogicalAnd[A](left: A ~> Boolean, right: A ~> Boolean) extends Lambda[A, Boolean]

  final case class LogicalOr[A](left: A ~> Boolean, right: A ~> Boolean) extends Lambda[A, Boolean]

  final case class LogicalNot[A](logic: A ~> Boolean) extends Lambda[A, Boolean]

  final case class SetScope[A](scope: Scope[A], ctx: ScopeContext) extends Lambda[A, Unit]

  final case class GetScope[A](scope: Scope[A], ctx: ScopeContext, value: A, schema: Schema[A]) extends Lambda[Any, A]

  sealed trait ScopeContext

  sealed trait Scope[A] {
    def set: A ~> Unit
    def get: Any ~> A
    def :=[X](f: X ~> A): X ~> Unit = set <<< f
  }

  object Scope {
    def make[A](value: A)(implicit ctx: ScopeContext, schema: Schema[A]): Scope[A] =
      new Scope[A] { self =>
        override def set: A ~> Unit = Lambda.SetScope(self, ctx)
        override def get: Any ~> A  = Lambda.GetScope(self, ctx, value, schema)
      }
  }
}
