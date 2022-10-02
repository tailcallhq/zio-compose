package compose.model

import java.util.concurrent.atomic.AtomicInteger

/**
 * A scope contains a set of references to values. These
 * values are available only within the scope and are
 * destroyed as soon as the scope ends. Scopes are
 * identified using an unique Id.
 */
sealed trait Scope {
  self =>
  def unsafe: Scope.Unsafe
}

object Scope {
  final case class Id(id: Int)
  private val idGen = new AtomicInteger(0)

  private[compose] def apply(): Scope = new Scope {
    override val unsafe: Unsafe = new Unsafe {
      override val id: Id = Id(idGen.incrementAndGet())
    }
  }

  trait Unsafe {
    def id: Id
  }
}
