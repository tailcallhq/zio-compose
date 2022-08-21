package compose.interpreter

import compose.ExecutionPlan.SourceExecution
import zio.{Task, ZIO}
import zio.schema.{DynamicValue, Schema}

trait SourceInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalSource(input: DynamicValue, operation: SourceExecution.Operation): Task[DynamicValue] = {
    operation match {
      case SourceExecution.Default(value)  => ZIO.succeed(value)
      case SourceExecution.FromMap(value)  =>
        ZIO.succeed(value.get(input) match {
          case Some(value) => DynamicValue.SomeValue(value)
          case None        => DynamicValue.NoneValue
        })
      case SourceExecution.Constant(value) => ZIO.succeed(value)
      case SourceExecution.WriteLine       =>
        for {
          string <- Interpreter.effect(input.toTypedValue(Schema[String]))
          _      <- zio.Console.printLine(string)
        } yield self.unit
    }
  }
}
