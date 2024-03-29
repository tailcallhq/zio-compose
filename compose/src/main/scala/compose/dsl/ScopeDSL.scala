package compose.dsl

import compose.ExecutionPlan.Scoped
import compose.model.Scope
import compose.{Lambda, ~>}

object ScopeDSL {

  trait Ctr {
    def scope[A, B](f: Scope => A ~> B): A ~> B = Lambda.unsafe.attempt[A, B] {
      val scope = Scope.unsafe.make
      Scoped.Using(f(scope).compile, scope)
    }
  }
}
