package compose.interpreter

import compose.ExecutionPlan
import compose.dsl.ArrowDSL.CanConcat
import compose.dsl.NumericDSL
import compose.dsl.NumericDSL.IsNumeric
import compose.interpreter.Interpreter.effect
import compose.ExecutionPlan.StringOperation
import zio.schema.{DynamicValue, Schema}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import zio.{Task, UIO, ZIO}
import zio.schema.codec.JsonCodec

final case class InMemory(scope: Scope[Int, Int, DynamicValue]) extends Interpreter {
  import InMemory._

  def evalDynamic(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue] = {
    plan match {
      case operation: ExecutionPlan.StringOperation =>
        for {
          string <- effect(input.toTypedValue(Schema[String]))
        } yield operation match {
          case StringOperation.Length    => Schema[Int].toDynamic(string.length)
          case StringOperation.UpperCase => Schema[String].toDynamic(string.toUpperCase)
          case StringOperation.LowerCase => Schema[String].toDynamic(string.toLowerCase)
        }

      case ExecutionPlan.EndScope(id) =>
        scope.delete(id).as(Schema[Unit].toDynamic {})

      case ExecutionPlan.Debug(name) =>
        val json = new String(JsonCodec.encode(Schema[DynamicValue])(input).toArray)
        ZIO.succeed(println(s"${name}: $json")).as(input)

      case ExecutionPlan.Arg(i, a1, a2) =>
        val s1 = a1.toSchema.asInstanceOf[Schema[Any]]
        val s2 = a2.toSchema.asInstanceOf[Schema[Any]]

        for {
          value  <- effect(input.toTypedValue(Schema.tuple2(s1, s2)))
          result <- i match {
            case 0 => ZIO.succeed(toDynamic(value._1)(s1))
            case 1 => ZIO.succeed(toDynamic(value._2)(s2))
            case n =>
              ZIO.fail(
                new RuntimeException(s"Can not extract element at index ${n} from ${value.getClass().getName()}"),
              )
          }
        } yield result

      case ExecutionPlan.GetScope(scopeId, ctxId, initial) =>
        for {
          option <- scope.get(scopeId, ctxId)
          value  <- option match {
            case Some(value) => ZIO.succeed(value)
            case None        => ZIO.succeed(initial)
          }
        } yield value

      case ExecutionPlan.SetScope(scopeId, ctxId) =>
        for {
          _ <- scope.set(scopeId, ctxId, input)
        } yield toDynamic(())

      case ExecutionPlan.RepeatWhile(f, cond) =>
        def loop(input: DynamicValue): Task[DynamicValue] = {
          for {
            output <- evalDynamic(f, input)
            isTrue <- eval[Boolean](cond, output)
            result <- if (isTrue) loop(output) else ZIO.succeed(output)
          } yield result
        }

        loop(input)

      case ExecutionPlan.DoWhile(f, cond) =>
        def loop: Task[DynamicValue] = {
          for {
            output <- evalDynamic(f, input)
            isTrue <- eval[Boolean](cond, input)
            result <- if (isTrue) loop else ZIO.succeed(output)
          } yield result
        }

        loop

      case ExecutionPlan.Concat(self, other, canConcat) =>
        for {
          canConcat <- effect(canConcat.toTypedValue(Schema[CanConcat[_]]))
          result    <- canConcat match {
            case CanConcat.ConcatString =>
              eval[String](self, input).zipWithPar(eval[String](other, input)) { case (a, b) =>
                toDynamic(a + b)
              }
          }
        } yield result

      case ExecutionPlan.Default(value) => ZIO.succeed(value)

      case ExecutionPlan.SetPath(path) =>
        input match {
          case DynamicValue.Tuple(DynamicValue.Record(values), input) =>
            def loop(
              path: List[String],
              values: ListMap[String, DynamicValue],
              a: DynamicValue,
            ): Either[Exception, DynamicValue] = {
              path match {
                case Nil          => Left(new Exception("Path not found"))
                case head :: tail =>
                  values.get(head) match {
                    case None    => Left(new Exception("Path not found"))
                    case Some(v) =>
                      if (tail.isEmpty) Right(DynamicValue.Record(values + (head -> a)))
                      else
                        loop(tail, v.asInstanceOf[DynamicValue.Record].values, a) map { value =>
                          DynamicValue.Record(values + (head -> value))
                        }
                  }
              }
            }
            ZIO.fromEither(loop(path, values, input))
          case input => ZIO.fail(new Exception(s"Set path doesn't work on: ${input}"))
        }

      case ExecutionPlan.LogicalAnd(left, right) =>
        for {
          left  <- eval[Boolean](left, input)
          right <- eval[Boolean](right, input)
        } yield toDynamic { left && right }

      case ExecutionPlan.LogicalOr(left, right) =>
        for {
          left  <- eval[Boolean](left, input)
          right <- eval[Boolean](right, input)
        } yield toDynamic { left || right }

      case ExecutionPlan.LogicalNot(plan)                             =>
        for {
          bool <- eval[Boolean](plan, input)
        } yield toDynamic { !bool }
      case ExecutionPlan.NumericOperation(operation, left, right, is) =>
        for {
          isNumeric <- effect(is.toTypedValue(Schema[IsNumeric[_]]))
          params    <-
            isNumeric match {
              case IsNumeric.NumericInt =>
                eval[Int](left, input).zip(eval[Int](right, input)).map { case (a, b) =>
                  operation match {
                    case NumericDSL.Operation.Add                => a + b
                    case NumericDSL.Operation.Multiply           => a * b
                    case NumericDSL.Operation.Subtract           => a - b
                    case NumericDSL.Operation.Divide             => a / b
                    case NumericDSL.Operation.GreaterThan        => if (a > b) 1 else 0
                    case NumericDSL.Operation.GreaterThanEqualTo => if (a >= b) 1 else 0
                  }
                }
            }
        } yield toDynamic(params)

      case ExecutionPlan.Zip(left, right) =>
        for {
          a <- evalDynamic(left, input)
          b <- evalDynamic(right, input)
        } yield DynamicValue.Tuple(a, b)

      case ExecutionPlan.IfElse(cond, ifTrue, ifFalse) =>
        for {
          cond   <- eval[Boolean](cond, input)
          result <- if (cond) evalDynamic(ifTrue, input) else evalDynamic(ifFalse, input)
        } yield result
      case ExecutionPlan.Pipe(first, second)           =>
        for {
          input  <- evalDynamic(first, input)
          output <- evalDynamic(second, input)
        } yield output
      case ExecutionPlan.GetPath(path)                 =>
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
      case ExecutionPlan.Equals(left, right)           =>
        for {
          left  <- evalDynamic(left, input)
          right <- evalDynamic(right, input)
        } yield toDynamic(left == right)

      case ExecutionPlan.FromMap(value, ast) =>
        val schema = ast.toSchema.asInstanceOf[Schema[Any]]
        for {
          result <- value.get(input) match {
            case Some(value) => effect(value.toTypedValue(schema)).map(Option(_))
            case None        => ZIO.succeed(None)
          }
        } yield Schema.option(schema).toDynamic(result)
      case ExecutionPlan.Constant(value)     => ZIO.succeed(value)
      case ExecutionPlan.Identity            => ZIO.succeed(input)
    }
  }
}

object InMemory {
  def make: UIO[InMemory] =
    Scope.inMemory[Int, Int, DynamicValue].map(scope => new InMemory(scope))

  private def toDynamic[A](a: A)(implicit schema: Schema[A]): DynamicValue =
    schema.toDynamic(a)
}
