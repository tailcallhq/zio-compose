package compose.interpreter

import compose.operation.DebugOp
import zio.schema.{DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.Task

trait DebugInterpreter {
  self: InMemoryInterpreter =>
  def evalDebug(input: DynamicValue, operation: DebugOp): Task[DynamicValue]  = {
    operation match {
      case DebugOp.Debug(plan, name) =>
        for {
          result <- evalDynamic(plan, input)
          json = new String(JsonCodec.encode(Schema[DynamicValue])(result).toArray)
          _ <- zio.Console.printLine(s"${name}: $json")
        } yield result
      case DebugOp.Show(plan, name)  =>
        val json = plan.json
        zio.Console.printLine(s"${name}: $json") *> evalDynamic(plan, input)
    }
  }
}
