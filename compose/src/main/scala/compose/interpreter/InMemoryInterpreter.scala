package compose.interpreter

import compose.operation.ScopeOp.{ContextId, ScopeId}
import compose.ExecutionPlan
import zio.schema.{DynamicValue, Schema}
import zio.{Task, UIO}

final case class InMemoryInterpreter(scope: Scope[ContextId, ScopeId, DynamicValue])
    extends Interpreter
    with ArrowInterpreter
    with DebugInterpreter
    with LogicalInterpreter
    with NumericInterpreter
    with OpticalInterpreter
    with RecursiveInterpreter
    with SourceInterpreter
    with ScopeInterpreter
    with StringInterpreter
    with TupleInterpreter {

  def evalDynamic(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue] = {
    plan match {
      case ExecutionPlan.DebugExecution(operation)         => evalDebug(input, operation)
      case ExecutionPlan.StringExecution(operation)        => evalString(input, operation)
      case ExecutionPlan.ScopeExecution(operation)         => evalScope(input, operation)
      case ExecutionPlan.TupleExecution(operation)         => evalTuple(input, operation)
      case ExecutionPlan.RecursiveExecution(operation)     => evalRecursive(input, operation)
      case ExecutionPlan.SourceExecution(operation)        => evalSource(input, operation)
      case ExecutionPlan.OpticalExecution(operation)       => evalOptical(input, operation)
      case ExecutionPlan.LogicalExecution(operation)       => evalLogical(input, operation)
      case ExecutionPlan.NumericExecution(operation, kind) => evalNumeric(input, operation, kind)
      case ExecutionPlan.ArrowExecution(operation)         => evalArrow(input, operation)

    }
  }
}

object InMemoryInterpreter {
  def make: UIO[InMemoryInterpreter] =
    Scope.inMemory[ContextId, ScopeId, DynamicValue].map(scope => new InMemoryInterpreter(scope))

  def toDynamic[A](a: A)(implicit schema: Schema[A]): DynamicValue =
    schema.toDynamic(a)
}
