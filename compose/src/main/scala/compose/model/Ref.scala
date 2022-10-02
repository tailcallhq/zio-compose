package compose.model

import compose.ExecutionPlan.Scoped
import compose.{Lambda, ~>}
import zio.schema.Schema

import java.util.concurrent.atomic.AtomicInteger

/**
 * Refs are mutable references to values. You can set and
 * get the value using Ref. Refs are automatically cleaned
 * up as soon as their containing scope is closed.
 */
sealed trait Ref[A] {
  final def :=[X](f: X ~> A): X ~> Unit = set <<< f
  def get: Any ~> A
  def set: A ~> Unit
  def id: Ref.Id
}

object Ref {
  final case class Id(id: Int, scope: Scope)

  private val idGen = new AtomicInteger(0)

  object unsafe {
    def make[A](value: A)(implicit scope: Scope, schema: Schema[A]): Ref[A] =
      new Ref[A] { self =>
        override def set: A ~> Unit = Lambda.unsafe.attempt[A, Unit] {
          Scoped.Set(self.id, scope)
        }

        override def get: Any ~> A = Lambda.unsafe.attempt[Any, A] {
          Scoped.Get(self.id, scope, schema.toDynamic(value))
        }

        override val id: Id = Id(idGen.incrementAndGet(), scope)
      }
  }

}
