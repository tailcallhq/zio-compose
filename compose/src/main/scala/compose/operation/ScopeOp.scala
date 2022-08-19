package compose.operation

import compose.ExecutionPlan
import zio.schema.DynamicValue

sealed trait ScopeOp
object ScopeOp {
  final case class SetScope(scopeId: ScopeId, ctxId: ContextId) extends ScopeOp

  final case class GetScope(scopeId: ScopeId, ctxId: ContextId, value: DynamicValue) extends ScopeOp

  final case class WithinScope(plan: ExecutionPlan, ctxId: ContextId) extends ScopeOp

  final case class ContextId(id: Int)

  final case class ScopeId(id: Int, contextId: ContextId)
}
