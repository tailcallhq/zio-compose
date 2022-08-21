package compose.interpreter

import compose.ExecutionPlan.DebugExecution
import zio.schema.{DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.Task

trait DebugInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalDebug(input: DynamicValue, operation: DebugExecution.Operation): Task[DynamicValue] = {
    operation match {
      case DebugExecution.Debug(plan, name) =>
        for {
          result <- evalDynamic(plan, input)
          json = new String(JsonCodec.encode(Schema[DynamicValue])(result).toArray)
          _ <- zio.Console.printLine(s"${name}: $json")
        } yield result
      case DebugExecution.Show(plan, name)  =>
        val json = plan.json
        zio.Console.printLine(s"${name}: $json") *> evalDynamic(plan, input)
    }
  }
}
