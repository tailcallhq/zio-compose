package compose.model

import compose.ExecutionPlan.Scoped
import compose.{Lambda, ~>}
import zio.schema.Schema

/**
 * Refs are mutable references to values. You can set and
 * get the value using Ref. Refs are automatically cleaned
 * up as soon as their containing scope is closed.
 */
final case class Ref[A](id: Ref.Id, value: A) {
  self =>
  def :=[X](f: X ~> A): X ~> Unit = set <<< f

  def get(implicit schema: Schema[A]): Any ~> A = Lambda.unsafe
    .attempt[Any, A] { Scoped.Get(self.id, id.scope, schema.toDynamic(value)) }

  def set: A ~> Unit = Lambda.unsafe.attempt[A, Unit] { Scoped.Set(self.id, id.scope) }
}

object Ref {
  final case class Id(id: String, scope: Scope)

  def make[A](key: String, value: A)(implicit scope: Scope): Ref[A] = Ref(Id(key, scope), value)
}
