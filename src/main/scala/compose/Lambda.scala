package compose

import zio.schema.Schema
import zio.Task

trait Lambda[-A, +B] { self =>

  final def ===[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean =
    ExecutionPlan.Equals(self.compile, other.compile).decompile

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

  final def ->>[I >: B, C](other: (C, I) ~> C)(implicit i: Schema[I]): Transformation[A, C] =
    self transform other

  final def +[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 = numOp(Numeric.Operation.Add, other)

  final def ++[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit ev: CanConcat[B1]): A1 ~> B1 =
    ExecutionPlan.Concat(self.compile, other.compile, Schema[CanConcat[_]].toDynamic(ev)).decompile

  final def *[A1 <: A, B1 >: B](other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 = numOp(Numeric.Operation.Multiply, other)

  final def *>[A1 <: A, B1 >: B, B2](
    other: Lambda[A1, B2],
  )(implicit
    b1: Schema[B1],
    b2: Schema[B2],
  ): A1 ~> B2 =
    ((self: A1 ~> B1) <*> other) >>> Lambda.arg1

  final def apply[A1 <: A, B1 >: B](a: A1)(implicit in: Schema[A1], out: Schema[B1]): Task[B1] =
    Interpreter.evalTyped[B1](self.compile, in.toDynamic(a))

  def compile: ExecutionPlan

  final def compose[X](other: Lambda[X, A]): Lambda[X, B] =
    other pipe self

  final def debug(name: String): Lambda[A, B] = ExecutionPlan.Debug(name, self.compile).decompile

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
    ExecutionPlan.Pipe(self.compile, other.compile).decompile

  final def repeatUntil[A1 <: A](cond: A1 ~> Boolean): A1 ~> B =
    ExecutionPlan.RepeatUntil(self.compile, cond.compile).decompile

  final def transform[I >: B, C](other: (C, I) ~> C)(implicit i: Schema[I]): Transformation[A, C] =
    Transformation[A, C, I](self, other)

  final def zip[A1 <: A, B1 >: B, B2](other: Lambda[A1, B2])(implicit
    b1: Schema[B1],
    b2: Schema[B2],
  ): A1 ~> (B1, B2) = ExecutionPlan.Combine(self.compile, other.compile, b1.ast, b2.ast).decompile

  private final def numOp[A1 <: A, B1 >: B](operation: Numeric.Operation, other: A1 ~> B1)(implicit
    num: IsNumeric[B1],
  ): A1 ~> B1 =
    ExecutionPlan.NumericOperation(operation, self.compile, other.compile, num.toDynamic).decompile

}

object Lambda {

  def arg0[A0, A1](implicit s0: Schema[A0], s1: Schema[A1]): (A0, A1) ~> A0 =
    ExecutionPlan.Arg(0, s0.ast, s1.ast).decompile

  def arg1[A0, A1](implicit s0: Schema[A0], s1: Schema[A1]): (A0, A1) ~> A1 =
    ExecutionPlan.Arg(1, s0.ast, s1.ast).decompile

  def constant[B](b: B)(implicit schema: Schema[B]): Any ~> B =
    ExecutionPlan.Constant(schema.toDynamic(b)).decompile

  def default[A](implicit schema: Schema[A]): Any ~> A =
    ExecutionPlan
      .Default(schema.defaultValue match {
        case Left(cause)  => throw new Exception(cause)
        case Right(value) => schema.toDynamic(value)
      })
      .decompile

  def fromMap[A, B](
    source: Map[A, B],
  )(implicit input: Schema[A], output: Schema[B]): Lambda[A, B] =
    Lambda(ExecutionPlan.FromMap(source.map { case (a, b) => (input.toDynamic(a), output.toDynamic(b)) }))

  def identity[A]: Lambda[A, A] = ExecutionPlan.Identity.decompile

  def ifElse[A, B](f: A ~> Boolean)(isTrue: A ~> B, isFalse: A ~> B): A ~> B =
    ExecutionPlan.IfElse(f.compile, isTrue.compile, isFalse.compile).decompile

  def scope[A, B](f: ScopeContext => A ~> B): A ~> B = f(new ScopeContext {})

  def seq[A](f: A ~> Unit*): A ~> Unit = f.reduceLeft(_ *> _)

  def transform[A, B](transformations: Transformation[A, B]*)(implicit b: Schema[B]): A ~> B = ExecutionPlan
    .Transform(
      transformations.map { case Transformation.Constructor(f, i, g) => (f.compile, i.ast, g.compile) }.toList,
      b.ast,
    )
    .decompile

  private[compose] def apply[A, B](plan: ExecutionPlan): Lambda[A, B] = new ~>[A, B] {
    override def compile: ExecutionPlan = plan
  }

  sealed trait ScopeContext

  sealed trait Scope[A] {
    def :=[X](f: X ~> A): X ~> Unit = set <<< f

    def get: Any ~> A

    def set: A ~> Unit
  }

  object Scope {
    def apply[A](value: A)(implicit ctx: ScopeContext, schema: Schema[A]): Scope[A] =
      new Scope[A] { self =>
        override def set: A ~> Unit = ExecutionPlan.SetScope(self.hashCode(), ctx.hashCode()).decompile
        override def get: Any ~> A  =
          ExecutionPlan.GetScope(self.hashCode(), ctx.hashCode(), schema.toDynamic(value), schema.ast).decompile
      }
  }
}
