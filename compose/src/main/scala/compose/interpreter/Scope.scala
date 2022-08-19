package compose.interpreter

import zio.{Ref, UIO}

trait Scope[S, K, V] {
  def delete(scope: S): UIO[Unit]
  def get(scope: S, key: K): UIO[Option[V]]
  def set(scope: S, key: K, value: V): UIO[Unit]
}

object Scope {
  def inMemory[C, S, V]: UIO[Scope[C, S, V]] = Ref.make(Map.empty[(C, S), V]).map(InMemory(_))

  final case class InMemory[C, S, V](ref: Ref[Map[(C, S), V]]) extends Scope[C, S, V] {
    def delete(scope: C): UIO[Unit] = ref.update { map => map.filter { case s -> _ -> _ => s != scope } }

    def get(scope: C, key: S): UIO[Option[V]] = ref.get.map(_.get(scope, key))

    def set(scope: C, key: S, value: V): UIO[Unit] = ref.update { map => map + (((scope, key), value)) }
  }
}
