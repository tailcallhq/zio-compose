package compose.dsl

import compose.{~>, Lambda}
import compose.ExecutionPlan.Scoped
import compose.ExecutionPlan.Scoped.{ContextId, RefId}
import zio.schema.Schema
import java.util.concurrent.atomic.AtomicInteger

trait ScopeDSL {
  def scope[A, B](f: ScopeContext => A ~> B): A ~> B = Lambda.make[A, B] {
    val ctx = ScopeContext()
    Scoped.WithinScope(f(ctx).compile, ctx.id)
  }

  sealed trait ScopeContext {
    self =>
    def id: ContextId = ContextId(ScopeContext.id.incrementAndGet)
  }

  object ScopeContext {
    private val id                             = new AtomicInteger(0)
    private[compose] def apply(): ScopeContext = new ScopeContext {}
  }

  sealed trait Scope[A] {
    def :=[X](f: X ~> A): X ~> Unit = set <<< f
    def get: Any ~> A
    def set: A ~> Unit
  }

  object Scope {
    private val id = new AtomicInteger(0)

    def make[A](value: A)(implicit ctx: ScopeContext, schema: Schema[A]): Scope[A] =
      new Scope[A] {
        self =>
        def id: RefId = RefId(Scope.id.incrementAndGet(), ctx.id)

        override def set: A ~> Unit = Lambda.make[A, Unit] {
          Scoped.SetScope(self.id, ctx.id)
        }

        override def get: Any ~> A = Lambda.make[Any, A] {
          Scoped.GetScope(self.id, ctx.id, schema.toDynamic(value))
        }
      }
  }
}
