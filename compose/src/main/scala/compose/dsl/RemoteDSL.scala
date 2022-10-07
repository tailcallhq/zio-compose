package compose.dsl

import compose.ExecutionPlan.Remote
import compose.model.http.{Request, Response}
import compose.{Lambda, ~>}

object RemoteDSL {
  trait Ctr {
    final def http: Request ~> Response = Lambda.unsafe.attempt[Request, Response] { Remote.Http }
  }
}
