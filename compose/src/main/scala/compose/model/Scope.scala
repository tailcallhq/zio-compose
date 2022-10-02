package compose.model

import java.util.concurrent.atomic.AtomicInteger

/**
 * A scope contains a set of references to values. These
 * values are available only within the scope and are
 * destroyed as soon as the scope ends. Scopes are
 * identified using an unique Id.
 */

case class Scope(id: Int)

object Scope {
  private val idGen = new AtomicInteger(0)

  object unsafe {
    def make: Scope = Scope(idGen.incrementAndGet())
  }
}
