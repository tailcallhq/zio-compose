package compose.interpreter

import compose.dsl.ArrowDSL.CanConcat
import compose.interpreter.Interpreter.effect
import compose.execution.ExecutionPlan.{LogicalOperation, NumericOperation, ScopeOperation, StringOperation}
import compose.execution.ExecutionPlan
import compose.execution.ExecutionPlan.ScopeOperation.{ContextId, ScopeId}
import zio.schema.{DynamicValue, Schema}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import zio.{Task, UIO, ZIO}
import zio.schema.codec.JsonCodec

final case class InMemoryInterpreter(scope: Scope[ContextId, ScopeId, DynamicValue]) extends Interpreter {
  import InMemoryInterpreter._

  def evalDynamic(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue] = {
    plan match {
      case ExecutionPlan.Show(plan, name) =>
        val json = plan.json
        zio.Console.printLine(s"${name}: $json") *> evalDynamic(plan, input)

      case operation: ExecutionPlan.StringOperation =>
        for {
          result <- operation match {
            case StringOperation.Length(plan) =>
              for {
                str <- eval[String](plan, input)
              } yield toDynamic(str.length)

            case StringOperation.UpperCase(plan) =>
              for {
                str <- eval[String](plan, input)
              } yield toDynamic(str.toUpperCase)

            case StringOperation.LowerCase(plan) =>
              for {
                str <- eval[String](plan, input)
              } yield toDynamic(str.toLowerCase)

            case StringOperation.StartsWith(self, other) =>
              for {
                str1 <- eval[String](self, input)
                str2 <- eval[String](other, input)
              } yield toDynamic(str1.startsWith(str2))

            case StringOperation.EndsWith(self, other) =>
              for {
                str1 <- eval[String](self, input)
                str2 <- eval[String](other, input)
              } yield toDynamic(str1.endsWith(str2))

            case StringOperation.Contains(self, other) =>
              for {
                str1 <- eval[String](self, input)
                str2 <- eval[String](other, input)
              } yield toDynamic(str1.contains(str2))
          }
        } yield result

      case ExecutionPlan.ScopeOperation(operation) =>
        operation match {
          case ScopeOperation.SetScope(scopeId, ctxId) =>
            for {
              _ <- scope.set(ctxId, scopeId, input)
            } yield toDynamic(())

          case ScopeOperation.GetScope(scopeId, ctxId, value) =>
            for {
              option <- scope.get(ctxId, scopeId)
              value  <- option match {
                case Some(value) => ZIO.succeed(value)
                case None        => ZIO.succeed(value)
              }
            } yield value

          case ScopeOperation.WithinScope(plan, ctxId) =>
            for {
              result <- evalDynamic(plan, input)
              _      <- scope.delete(ctxId)
            } yield result
        }

      case ExecutionPlan.Debug(plan, name) =>
        for {
          result <- evalDynamic(plan, input)
          json = new String(JsonCodec.encode(Schema[DynamicValue])(result).toArray)
          _ <- zio.Console.printLine(s"${name}: $json")
        } yield result

      case ExecutionPlan.Arg(plan, i) =>
        for {
          input  <- evalDynamic(plan, input)
          result <- input match {
            case DynamicValue.Tuple(left, right) =>
              i match {
                case 0 => ZIO.succeed(left)
                case 1 => ZIO.succeed(right)
                case n =>
                  ZIO.fail(
                    new RuntimeException(s"Can not extract element at index ${n} from ${input}"),
                  )
              }
            case _ => ZIO.fail(new RuntimeException(s"Can not extract args from ${input}"))
          }
        } yield result

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

      case ExecutionPlan.LogicalOperation(operation) =>
        operation match {
          case LogicalOperation.And(left, right) =>
            for {
              left  <- eval[Boolean](left, input)
              right <- eval[Boolean](right, input)
            } yield toDynamic {
              left && right
            }
          case LogicalOperation.Or(left, right)  =>
            for {
              left  <- eval[Boolean](left, input)
              right <- eval[Boolean](right, input)
            } yield toDynamic {
              left || right
            }

          case LogicalOperation.Not(plan) =>
            for {
              bool <- eval[Boolean](plan, input)
            } yield toDynamic(!bool)

          case LogicalOperation.Equals(left, right) =>
            for {
              left  <- evalDynamic(left, input)
              right <- evalDynamic(right, input)
            } yield toDynamic(left == right)

        }

      case ExecutionPlan.NumericOperation(operation, ExecutionPlan.NumericOperation.Kind.IntNumber) =>
        operation match {
          case NumericOperation.Add(left, right)                =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a + b)
          case NumericOperation.Multiply(left, right)           =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a * b)
          case NumericOperation.Divide(left, right)             =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a / b)
          case NumericOperation.GreaterThan(left, right)        =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a > b)
          case NumericOperation.GreaterThanEqualTo(left, right) =>
            for {
              a <- eval[Int](left, input)
              b <- eval[Int](right, input)
            } yield toDynamic(a >= b)
          case NumericOperation.Negate(plan)                    =>
            for {
              a <- eval[Int](plan, input)
            } yield toDynamic(-a)
        }

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

      case ExecutionPlan.FromMap(value)  =>
        ZIO.succeed(value.get(input) match {
          case Some(value) => DynamicValue.SomeValue(value)
          case None        => DynamicValue.NoneValue
        })
      case ExecutionPlan.Constant(value) => ZIO.succeed(value)
      case ExecutionPlan.Identity        => ZIO.succeed(input)
    }
  }
}

object InMemoryInterpreter {
  def make: UIO[InMemoryInterpreter] =
    Scope.inMemory[ContextId, ScopeId, DynamicValue].map(scope => new InMemoryInterpreter(scope))

  private def toDynamic[A](a: A)(implicit schema: Schema[A]): DynamicValue =
    schema.toDynamic(a)
}
