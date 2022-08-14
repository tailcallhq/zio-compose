package compose.interpreter

import zio.{Ref, UIO}

trait Scope[S, K, V] {
  def delete(scope: S): UIO[Unit]
  def get(scope: S, key: K): UIO[Option[V]]
  def set(scope: S, key: K, value: V): UIO[Unit]
}

object Scope {
  def inMemory[S, K, V]: UIO[Scope[S, K, V]] = Ref.make(Map.empty[(S, K), V]).map(InMemory(_))

  final case class InMemory[S, K, V](ref: Ref[Map[(S, K), V]]) extends Scope[S, K, V] {
    def delete(scope: S): UIO[Unit] = ref.update { map => map.filter { case s -> _ -> _ => s != scope } }

    def get(scope: S, key: K): UIO[Option[V]] = ref.get.map(_.get(scope, key))

    def set(scope: S, key: K, value: V): UIO[Unit] = ref.update { map => map + (((scope, key), value)) }
  }
}
