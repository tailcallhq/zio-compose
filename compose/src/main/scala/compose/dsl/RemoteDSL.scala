package compose.dsl

import compose.ExecutionPlan.Remote
import compose.model.http.{Request, Response}
import compose.{Lambda, ~>}

trait RemoteDSL {
  final def http: Request ~> Response = Lambda.unsafe.attempt[Request, Response] { Remote.Http }
}
