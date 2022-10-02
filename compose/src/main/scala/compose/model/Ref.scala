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

  private[compose] def unsafe: Ref.Unsafe
}

object Ref {

  final case class Id(id: Int, scopeId: Scope.Id)

  trait Unsafe {
    def id: Id
  }

  private val idGen = new AtomicInteger(0)

  def unsafeMake[A](value: A)(implicit scope: Scope, schema: Schema[A]): Ref[A] =
    new Ref[A] {
      self =>
      override def set: A ~> Unit = Lambda.unsafe.attempt[A, Unit] {
        Scoped.SetScope(self.unsafe.id, scope.unsafe.id)
      }

      override def get: Any ~> A = Lambda.unsafe.attempt[Any, A] {
        Scoped.GetScope(self.unsafe.id, scope.unsafe.id, schema.toDynamic(value))
      }

      override lazy val unsafe: Unsafe = new Unsafe {
        lazy val id: Id = Id(idGen.incrementAndGet(), scope.unsafe.id)
      }
    }
}
