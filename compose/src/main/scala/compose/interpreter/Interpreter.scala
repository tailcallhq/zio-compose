package compose.interpreter

import zio.{Task, UIO, ZIO}
import zio.schema.{DynamicValue, Schema}
import compose.{~>, ExecutionPlan}
import compose.ExecutionPlan.ScopeExecution.{ContextId, RefId}
import zio.schema.codec.JsonCodec

trait Interpreter {
  def eval[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[B] =
    eval[B](lmb.compile, Schema.primitive[Unit].toDynamic(()))

  def eval[A](plan: ExecutionPlan, value: DynamicValue)(implicit ev: Schema[A]): Task[A] =
    evalDynamic(plan, value).flatMap(value => Interpreter.effect(value.toTypedValue(ev)))

  def evalDynamic[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[DynamicValue] =
    evalDynamic(lmb.compile, Schema.primitive[Unit].toDynamic(()))

  def evalDynamic(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue]

  def evalJson[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[String] =
    evalDynamic(lmb).map(res => new String(JsonCodec.encode(Schema[DynamicValue])(res).toArray))
}

object Interpreter {
  def effect[E, A](e: Either[String, A])(implicit trace: zio.Trace): Task[A] =
    e match {
      case Left(error) => ZIO.fail(new Exception(error))
      case Right(a)    => ZIO.succeed(a)
    }

  def eval[B](f: Any ~> B)(implicit ev: Schema[B]): Task[B] =
    for {
      int <- Interpreter.inMemory
      res <- int.eval(f)
    } yield res

  def inMemory: UIO[Interpreter] =
    Scope.inMemory[ContextId, RefId, DynamicValue].map(scope => InMemoryInterpreter(scope))

  final case class InMemoryInterpreter(scope: Scope[ContextId, RefId, DynamicValue])
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

    def unit: DynamicValue = Schema.primitive[Unit].toDynamic(())

    def toDynamic[A](a: A)(implicit schema: Schema[A]): DynamicValue =
      schema.toDynamic(a)
  }
}
