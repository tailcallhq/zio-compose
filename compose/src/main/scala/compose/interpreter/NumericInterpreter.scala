package compose.interpreter
import compose.interpreter.InMemoryInterpreter.toDynamic
import compose.operation.NumericOp
import compose.operation.NumericOp.Kind
import zio.schema.DynamicValue
import zio.Task

trait NumericInterpreter {
  self: InMemoryInterpreter =>
  def evalNumeric(input: DynamicValue, operation: NumericOp, kind: NumericOp.Kind): Task[DynamicValue] = {
    kind match {
      case Kind.IntNumber =>
        operation match {
          case NumericOp.Add(left, right)                =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a + b)
          case NumericOp.Multiply(left, right)           =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a * b)
          case NumericOp.Divide(left, right)             =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a / b)
          case NumericOp.GreaterThan(left, right)        =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a > b)
          case NumericOp.GreaterThanEqualTo(left, right) =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a >= b)
          case NumericOp.Negate(plan)                    =>
            for {
              a <- eval[Int](plan, input)
            } yield toDynamic(-a)
        }
    }

  }
}
