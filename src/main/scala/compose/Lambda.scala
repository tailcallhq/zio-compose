package compose

import compose.dsl.{ArrowDSL, BooleanDSL, NumericDSL, TupleDSL}
import compose.Lambda.{unsafeMake, ScopeContext}
import zio.schema.Schema
import zio.Task

trait Lambda[-A, +B] extends ArrowDSL[A, B] with NumericDSL[A, B] with TupleDSL[A, B] with BooleanDSL[A, B] { self =>
  final def ->>[I >: B, C](other: (C, I) ~> C)(implicit i: Schema[I]): Transformation[A, C] =
    self transform other

  def compile: ExecutionPlan

  final def debug(name: String): Lambda[A, B] = unsafeMake {
    ExecutionPlan.Debug(name, self.compile)
  }

  final def endContext(ctx: ScopeContext): A ~> B =
    unsafeMake {
      ExecutionPlan.EndScope(ctx.hashCode())
    }

  final def eval[A1 <: A, B1 >: B](a: A1)(implicit in: Schema[A1], out: Schema[B1]): Task[B1] =
    Interpreter.evalTyped[B1](self.compile, in.toDynamic(a))

  final def repeatWhile[B1 >: B <: A](cond: B1 ~> Boolean): B1 ~> B1 = unsafeMake {
    ExecutionPlan.RepeatWhile(self.compile, cond.compile)
  }

  final def transform[I >: B, C](other: (C, I) ~> C)(implicit i: Schema[I]): Transformation[A, C] =
    Transformation[A, C, I](self, other)
}

object Lambda {

  def constant[B](b: B)(implicit schema: Schema[B]): Any ~> B = unsafeMake {
    ExecutionPlan.Constant(schema.toDynamic(b))
  }

  def default[A](implicit schema: Schema[A]): Any ~> A = unsafeMake {
    ExecutionPlan
      .Default(schema.defaultValue match {
        case Left(cause)  => throw new Exception(cause)
        case Right(value) => schema.toDynamic(value)
      })
  }

  def fromMap[A, B](
    source: Map[A, B],
  )(implicit input: Schema[A], output: Schema[B]): Lambda[A, Option[B]] =
    Lambda.unsafeMake(
      ExecutionPlan.FromMap(source.map { case (a, b) => (input.toDynamic(a), output.toDynamic(b)) }, output.ast),
    )

  def identity[A]: Lambda[A, A] = unsafeMake {
    ExecutionPlan.Identity
  }

  def scope[A, B](f: ScopeContext => A ~> B): A ~> B = Lambda.unsafeMake {
    val context = ScopeContext()
    f(context).endContext(context).compile
  }

  def stats[A](f: A ~> Unit*): A ~> Unit = f.reduceLeft(_ *> _)

  def transform[A, B](transformations: Transformation[A, B]*)(implicit s: Schema[B]): A ~> B =
    transformations.foldLeft[A ~> B](default[B]) {
      case (ab, transformation: Transformation.Constructor[A @unchecked, B @unchecked, Any @unchecked]) =>
        transformation(ab)
    }

  private[compose] def unsafeMake[A, B](plan: => ExecutionPlan): Lambda[A, B] = new ~>[A, B] {
    override def compile: ExecutionPlan = plan
  }

  sealed trait ScopeContext

  sealed trait Scope[A] {
    def :=[X](f: X ~> A): X ~> Unit = set <<< f
    def get: Any ~> A
    def set: A ~> Unit
  }

  object ScopeContext {
    private[compose] def apply(): ScopeContext = new ScopeContext {}
  }

  object Scope {
    def apply[A](value: A)(implicit ctx: ScopeContext, schema: Schema[A]): Scope[A] =
      new Scope[A] { self =>
        override def set: A ~> Unit = unsafeMake {
          ExecutionPlan.SetScope(self.hashCode(), ctx.hashCode())
        }

        override def get: Any ~> A = unsafeMake {
          ExecutionPlan.GetScope(self.hashCode(), ctx.hashCode(), schema.toDynamic(value))
        }
      }
  }
}
