package compose.interpreter

import compose.ExecutionPlan.NumericExecution
import zio.schema.DynamicValue
import zio.Task

trait NumericInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalNumeric(
    input: DynamicValue,
    operation: NumericExecution.Operation,
    kind: NumericExecution.Kind,
  ): Task[DynamicValue] = {
    kind match {
      case NumericExecution.Kind.IntNumber =>
        operation match {
          case NumericExecution.Add(left, right)                =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a + b)
          case NumericExecution.Multiply(left, right)           =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a * b)
          case NumericExecution.Divide(left, right)             =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a / b)
          case NumericExecution.GreaterThan(left, right)        =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a > b)
          case NumericExecution.GreaterThanEqualTo(left, right) =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a >= b)
          case NumericExecution.Negate(plan)                    =>
            for {
              a <- eval[Int](plan, input)
            } yield toDynamic(-a)
        }
    }

  }
}
