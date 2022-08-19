package compose

import compose.dsl._
import compose.lens.Transformation
import compose.Lambda.make
import compose.execution.ExecutionPlan
import compose.execution.ExecutionPlan.ScopeOperation
import compose.execution.ExecutionPlan.ScopeOperation.{ContextId, ScopeId}
import zio.schema.Schema

trait Lambda[-A, +B]
    extends ArrowDSL[A, B]
    with NumericDSL[A, B]
    with TupleDSL[A, B]
    with BooleanDSL[A, B]
    with StringDSL[A, B] { self =>

  final def ->>[I >: B, C](other: (C, I) ~> C)(implicit i: Schema[I]): Transformation[A, C] =
    self transform other

  def compile: ExecutionPlan

  final def debug[B1 >: B](name: String)(implicit s: Schema[B1]): A ~> B1 =
    make[A, B1] { ExecutionPlan.Debug(self.compile, name) }

  final def doUntil[C](cond: C ~> Boolean): A ~> B =
    doWhile(cond.not)

  final def narrow[A1](implicit ev: A1 <:< A): A1 ~> B = self.asInstanceOf[A1 ~> B]

  final def widen[B1](implicit ev: B <:< B1): A ~> B1 = self.asInstanceOf[A ~> B1]

}

object Lambda {

  def constant[B](b: B)(implicit schema: Schema[B]): Any ~> B =
    make[Any, B] { ExecutionPlan.Constant(schema.toDynamic(b)) }

  def default[A](implicit schema: Schema[A]): Any ~> A = make[Any, A] {
    ExecutionPlan
      .Default(schema.defaultValue match {
        case Left(cause)  => throw new Exception(cause)
        case Right(value) => schema.toDynamic(value)
      })
  }

  def fromMap[A, B](
    source: Map[A, B],
  )(implicit input: Schema[A], output: Schema[B]): Lambda[A, Option[B]] =
    Lambda.make[A, Option[B]](
      ExecutionPlan.FromMap(source.map { case (a, b) => (input.toDynamic(a), output.toDynamic(b)) }),
    )

  def identity[A]: Lambda[A, A] = make[A, A] { ExecutionPlan.Identity }

  def scope[A, B](f: ScopeContext => A ~> B)(implicit s: Schema[B]): A ~> B = Lambda.make[A, B] {
    val ctx = ScopeContext()
    ScopeOperation(ScopeOperation.WithinScope(f(ctx).compile, ctx.id))
  }

  def stats[A, B](f: A ~> B*)(implicit ev: Schema[B]): A ~> B = f.reduce(_ *> _)

  def transform[A, B](transformations: Transformation[A, B]*)(implicit s: Schema[B]): A ~> B =
    transformations.foldLeft[A ~> B](default[B]) {
      case (ab, transformation: Transformation.Constructor[A @unchecked, B @unchecked, Any @unchecked]) =>
        transformation(ab)
    }

  sealed trait ScopeContext { self =>
    def id: ContextId = ContextId(self.hashCode())
  }

  sealed trait Scope[A] {
    def :=[X](f: X ~> A): X ~> Unit = set <<< f
    def get: Any ~> A
    def set: A ~> Unit
  }

  trait UnsafeMake[A, B] {
    def apply(plan: ExecutionPlan): A ~> B = new ~>[A, B] {
      override def compile: ExecutionPlan = plan
    }
  }

  object ScopeContext {
    private[compose] def apply(): ScopeContext = new ScopeContext {}
  }

  object Scope {

    def make[A](value: A)(implicit ctx: ScopeContext, schema: Schema[A]): Scope[A] =
      new Scope[A] { self =>
        def id: ScopeId = ScopeId(self.hashCode(), ctx.id)

        override def set: A ~> Unit = Lambda.make[A, Unit] {
          ScopeOperation(ScopeOperation.SetScope(self.id, ctx.id))
        }

        override def get: Any ~> A = Lambda.make[Any, A] {
          ScopeOperation(
            ScopeOperation.GetScope(self.id, ctx.id, schema.toDynamic(value)),
          )
        }
      }
  }

  private[compose] object make {
    def apply[A, B]: UnsafeMake[A, B] = new UnsafeMake[A, B] {}
  }
}
