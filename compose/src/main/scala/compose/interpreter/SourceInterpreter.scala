package compose.interpreter

import compose.operation.SourceOp
import zio.schema.DynamicValue
import zio.{Task, ZIO}

trait SourceInterpreter {
  self: InMemoryInterpreter =>
  def evalSource(input: DynamicValue, operation: SourceOp) : Task[DynamicValue] = {
    operation match {
      case SourceOp.Default(value)  => ZIO.succeed(value)
      case SourceOp.FromMap(value)  =>
        ZIO.succeed(value.get(input) match {
          case Some(value) => DynamicValue.SomeValue(value)
          case None        => DynamicValue.NoneValue
        })
      case SourceOp.Constant(value) => ZIO.succeed(value)
    }
  }
}
