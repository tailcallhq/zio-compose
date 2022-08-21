package compose.dsl

import compose.{~>, Lambda}
import compose.ExecutionPlan.ScopeExecution
import compose.ExecutionPlan.ScopeExecution.{ContextId, RefId}
import zio.schema.Schema

trait ScopeDSL {
  def scope[A, B](f: ScopeContext => A ~> B)(implicit s: Schema[B]): A ~> B = Lambda.make[A, B] {
    val ctx = ScopeContext()
    ScopeExecution(ScopeExecution.WithinScope(f(ctx).compile, ctx.id))
  }

  sealed trait ScopeContext {
    self =>
    def id: ContextId = ContextId(self.hashCode())
  }

  object ScopeContext {
    private[compose] def apply(): ScopeContext = new ScopeContext {}
  }

  sealed trait Scope[A] {
    def :=[X](f: X ~> A): X ~> Unit = set <<< f
    def get: Any ~> A
    def set: A ~> Unit
  }

  object Scope {
    def make[A](value: A)(implicit ctx: ScopeContext, schema: Schema[A]): Scope[A] =
      new Scope[A] {
        self =>
        def id: RefId = RefId(self.hashCode(), ctx.id)

        override def set: A ~> Unit = Lambda.make[A, Unit] {
          ScopeExecution(ScopeExecution.SetScope(self.id, ctx.id))
        }

        override def get: Any ~> A = Lambda.make[Any, A] {
          ScopeExecution(
            ScopeExecution.GetScope(self.id, ctx.id, schema.toDynamic(value)),
          )
        }
      }
  }
}
