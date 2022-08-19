package compose.interpreter

import compose.operation.StringOp
import zio.schema.DynamicValue
import zio.Task

trait StringInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalString(input: DynamicValue, operation: StringOp): Task[DynamicValue] = {
    operation match {
      case StringOp.Length(plan) =>
        for {
          str <- eval[String](plan, input)
        } yield toDynamic(str.length)

      case StringOp.UpperCase(plan) =>
        for {
          str <- eval[String](plan, input)
        } yield toDynamic(str.toUpperCase)

      case StringOp.LowerCase(plan) =>
        for {
          str <- eval[String](plan, input)
        } yield toDynamic(str.toLowerCase)

      case StringOp.StartsWith(self, other) =>
        for {
          str1 <- eval[String](self, input)
          str2 <- eval[String](other, input)
        } yield toDynamic(str1.startsWith(str2))

      case StringOp.EndsWith(self, other) =>
        for {
          str1 <- eval[String](self, input)
          str2 <- eval[String](other, input)
        } yield toDynamic(str1.endsWith(str2))

      case StringOp.Contains(self, other) =>
        for {
          str1 <- eval[String](self, input)
          str2 <- eval[String](other, input)
        } yield toDynamic(str1.contains(str2))

      case StringOp.Concat(self, other) =>
        for {
          str1 <- eval[String](self, input)
          str2 <- eval[String](other, input)
        } yield toDynamic(str1 ++ str2)
    }
  }
}
