package compose

import zio.schema.ast.SchemaAst
import zio.{Task, ZIO}
import zio.schema.{DynamicValue, Schema}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object Interpreter {

  def evalDynamic(plan: ExecutionPlan, value: DynamicValue): Task[DynamicValue] =
    Interpreter.eval(plan, value)

  def evalTyped[A](plan: ExecutionPlan, value: DynamicValue)(implicit ev: Schema[A]): Task[A] =
    evalDynamic(plan, value).flatMap(value => effect(value.toTypedValue(ev)))

  def eval(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue] =
    plan match {
      case ExecutionPlan.LogicalOperation(operation, left, right)     =>
        for {
          left  <- evalTyped[Boolean](left, input)
          right <- evalTyped[Boolean](right, input)
        } yield encode {
          operation match {
            case Logical.And => left && right
            case Logical.Or  => left || right
          }
        }
      case ExecutionPlan.NumericOperation(operation, left, right, is) =>
        for {
          isNumeric <- effect(is.toTypedValue(Schema[IsNumeric[_]]))
          params    <-
            isNumeric match {
              case IsNumeric.NumericInt =>
                evalTyped[Int](left, input).zip(evalTyped[Int](right, input)).map { case (a, b) =>
                  operation match {
                    case Numeric.Operation.Add      => a + b
                    case Numeric.Operation.Multiply => a * b
                    case Numeric.Operation.Subtract => a - b
                    case Numeric.Operation.Divide   => a / b
                  }
                }
            }
        } yield encode(params)

      case ExecutionPlan.Combine(left, right, o1, o2) =>
        eval(left, input).zipPar(eval(right, input)).map { case (a, b) =>
          encode(merge(a, b, o1, o2))
        }

      case ExecutionPlan.IfElse(cond, ifTrue, ifFalse) =>
        for {
          cond   <- evalTyped[Boolean](cond, input)
          result <- if (cond) eval(ifTrue, input) else eval(ifFalse, input)
        } yield result
      case ExecutionPlan.Pipe(first, second)           =>
        for {
          input  <- eval(first, input)
          output <- eval(second, input)
        } yield output
      case ExecutionPlan.Select(path)                  =>
        input match {
          case DynamicValue.Record(values) =>
            @tailrec
            def loop(path: List[String], values: ListMap[String, DynamicValue]): Either[Exception, DynamicValue] = {
              path match {
                case Nil          => Left(new Exception("Path not found"))
                case head :: tail =>
                  values.get(head) match {
                    case None    => Left(new Exception("Path not found"))
                    case Some(v) =>
                      if (tail.isEmpty) Right(v)
                      else
                        loop(tail, v.asInstanceOf[DynamicValue.Record].values)
                  }
              }
            }
            ZIO.fromEither(loop(path, values))
          case _                           => ZIO.fail(new Exception("Select only works on records"))
        }
      case ExecutionPlan.Equals(left, right)           => ZIO.succeed(encode(left == right))
      case ExecutionPlan.FromMap(value)                =>
        value.get(input) match {
          case Some(v) => ZIO.succeed(v)
          case None    =>
            ZIO.fail(new Exception("Key lookup failed in dictionary"))
        }
      case ExecutionPlan.Constant(value)               => ZIO.succeed(value)
      case ExecutionPlan.Identity                      => ZIO.succeed(input)
    }

  private def effect[A](e: Either[String, A]): Task[A] =
    e match {
      case Left(error) => ZIO.fail(new Exception(error))
      case Right(a)    => ZIO.succeed(a)
    }

  private def encode[A](a: A)(implicit schema: Schema[A]): DynamicValue =
    schema.toDynamic(a)

  private def merge(
    d1: DynamicValue,
    d2: DynamicValue,
    a1: SchemaAst,
    a2: SchemaAst,
  ): Either[String, DynamicValue] = {
    val s1 = a1.toSchema.asInstanceOf[Schema[Any]]
    val s2 = a2.toSchema.asInstanceOf[Schema[Any]]
    for {
      v1 <- d1.toTypedValue(s1)
      v2 <- d2.toTypedValue(s2)
    } yield (s1 <*> s2).toDynamic((v1, v2))
  }
}
