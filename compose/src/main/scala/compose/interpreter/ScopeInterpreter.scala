package compose.interpreter

import compose.operation.ScopeOp
import zio.schema.DynamicValue
import zio.{Task, ZIO}

trait ScopeInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalScope(input: DynamicValue, operation: ScopeOp): Task[DynamicValue] = {
    operation match {
      case ScopeOp.SetScope(scopeId, ctxId) =>
        for {
          _ <- scope.set(ctxId, scopeId, input)
        } yield toDynamic(())

      case ScopeOp.GetScope(scopeId, ctxId, value) =>
        for {
          option <- scope.get(ctxId, scopeId)
          value  <- option match {
            case Some(value) => ZIO.succeed(value)
            case None        => ZIO.succeed(value)
          }
        } yield value

      case ScopeOp.WithinScope(plan, ctxId) =>
        for {
          result <- evalDynamic(plan, input)
          _      <- scope.delete(ctxId)
        } yield result
    }
  }
}
