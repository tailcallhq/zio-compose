package compose.interpreter

import compose.ExecutionPlan.ScopeExecution
import zio.schema.DynamicValue
import zio.{Task, ZIO}

trait ScopeInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalScope(input: DynamicValue, operation: ScopeExecution.Operation): Task[DynamicValue] = {
    operation match {
      case ScopeExecution.SetScope(refId, ctxId) =>
        for {
          _ <- scope.set(ctxId, refId, input)
        } yield toDynamic(())

      case ScopeExecution.GetScope(refId, ctxId, value) =>
        for {
          option <- scope.get(ctxId, refId)
          value  <- option match {
            case Some(value) => ZIO.succeed(value)
            case None        => ZIO.succeed(value)
          }
        } yield value

      case ScopeExecution.WithinScope(plan, ctxId) =>
        for {
          result <- evalDynamic(plan, input)
          _      <- scope.delete(ctxId)
        } yield result
    }
  }
}
