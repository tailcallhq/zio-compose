package compose.interpreter

import compose.ExecutionPlan.StringExecution
import zio.schema.DynamicValue
import zio.Task

trait StringInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalString(input: DynamicValue, operation: StringExecution.Operation): Task[DynamicValue] = {
    operation match {
      case StringExecution.Length(plan) =>
        for {
          str <- eval[String](plan, input)
        } yield toDynamic(str.length)

      case StringExecution.UpperCase(plan) =>
        for {
          str <- eval[String](plan, input)
        } yield toDynamic(str.toUpperCase)

      case StringExecution.LowerCase(plan) =>
        for {
          str <- eval[String](plan, input)
        } yield toDynamic(str.toLowerCase)

      case StringExecution.StartsWith(self, other) =>
        for {
          str1 <- eval[String](self, input)
          str2 <- eval[String](other, input)
        } yield toDynamic(str1.startsWith(str2))

      case StringExecution.EndsWith(self, other) =>
        for {
          str1 <- eval[String](self, input)
          str2 <- eval[String](other, input)
        } yield toDynamic(str1.endsWith(str2))

      case StringExecution.Contains(self, other) =>
        for {
          str1 <- eval[String](self, input)
          str2 <- eval[String](other, input)
        } yield toDynamic(str1.contains(str2))

      case StringExecution.Concat(self, other) =>
        for {
          str1 <- eval[String](self, input)
          str2 <- eval[String](other, input)
        } yield toDynamic(str1 ++ str2)
    }
  }
}
