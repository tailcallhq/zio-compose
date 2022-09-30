package compose.dsl
import compose._

object DebugDSL {
  trait Op[-A, +B] {
    self: A ~> B =>

    import ExecutionPlan._

    final def debug[B1 >: B](name: String): A ~> B1 = Lambda.unsafe.attempt[A, B1] {
      Debugger.Debug(self.compile, name)
    }

    final def show(name: String): A ~> B = Lambda.unsafe.attempt[A, B](Debugger.Show(self.compile, name))

    final def address: Any ~> String = Lambda.unsafe.attempt[Any, String] {
      Debugger.Address(self.compile)
    }
  }
}
