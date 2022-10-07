package compose.dsl

import compose.Lambda.constant
import compose._

object ConsoleDSL {
  trait Ctr {
    def writeLine: String ~> Unit = Lambda.unsafe
      .attempt[String, Unit] { ExecutionPlan.Console.WriteLine }

    def writeLine(text: String): Any ~> Unit = constant(text) >>> writeLine

    def readLine: Any ~> String = Lambda.unsafe
      .attempt[Any, String] { ExecutionPlan.Console.ReadLine(None) }

    def readLine(prompt: String): Any ~> String = Lambda.unsafe
      .attempt[Any, String] { ExecutionPlan.Console.ReadLine(Some(prompt)) }
  }
}
