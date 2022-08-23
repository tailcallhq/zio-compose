package compose

import zio.{Ref, UIO}

trait ScopeContext[S, K, V] {
  def delete(scope: S): UIO[Unit]
  def get(scope: S, key: K): UIO[Option[V]]
  def set(scope: S, key: K, value: V): UIO[Unit]
}

object ScopeContext {
  def inMemory[C, S, V]: UIO[ScopeContext[C, S, V]] = Ref.make(Map.empty[(C, S), V]).map(InMemory(_))

  final case class InMemory[C, S, V](ref: Ref[Map[(C, S), V]]) extends ScopeContext[C, S, V] {
    def delete(scope: C): UIO[Unit] = ref.update { map => map.filter { case s -> _ -> _ => s != scope } }

    def get(scope: C, key: S): UIO[Option[V]] = ref.get.map(_.get(scope, key))

    def set(scope: C, key: S, value: V): UIO[Unit] = ref.update { map => map + (((scope, key), value)) }
  }
}
