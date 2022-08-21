package compose.interpreter

import compose.operation.SourceOp
import zio.schema.DynamicValue
import zio.{Task, ZIO}
import zio.schema.Schema

trait SourceInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalSource(input: DynamicValue, operation: SourceOp): Task[DynamicValue] = {
    operation match {
      case SourceOp.Default(value)  => ZIO.succeed(value)
      case SourceOp.FromMap(value)  =>
        ZIO.succeed(value.get(input) match {
          case Some(value) => DynamicValue.SomeValue(value)
          case None        => DynamicValue.NoneValue
        })
      case SourceOp.Constant(value) => ZIO.succeed(value)
      case SourceOp.WriteLine       =>
        for {
          string <- Interpreter.effect(input.toTypedValue(Schema[String]))
          _      <- zio.Console.printLine(string)
        } yield self.unit
    }
  }
}
