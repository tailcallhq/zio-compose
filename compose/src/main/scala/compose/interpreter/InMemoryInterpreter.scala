package compose.interpreter

import compose.operation._
import compose.operation.ScopeOp.{ContextId, ScopeId}
import compose.ExecutionPlan
import zio.schema.{DynamicValue, Schema}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import zio.{Task, UIO, ZIO}
import zio.schema.codec.JsonCodec

final case class InMemoryInterpreter(scope: Scope[ContextId, ScopeId, DynamicValue]) extends Interpreter {
  import InMemoryInterpreter._

  def evalDynamic(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue] = {
    plan match {
      case ExecutionPlan.DebugExecution(operation) =>
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

      case ExecutionPlan.StringExecution(operation) =>
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

      case ExecutionPlan.ScopeExecution(operation) =>
        operation match {
          case ScopeOp.SetScope(scopeId, ctxId) =>
            for {
              _ <- scope.set(ctxId, scopeId, input)
            } yield toDynamic(())

          case ScopeOp.GetScope(scopeId, ctxId, value) =>
            for {
              option <- scope.get(ctxId, scopeId)
              value  <- option match {
                case Some(value) => ZIO.succeed(value)
                case None        => ZIO.succeed(value)
              }
            } yield value

          case ScopeOp.WithinScope(plan, ctxId) =>
            for {
              result <- evalDynamic(plan, input)
              _      <- scope.delete(ctxId)
            } yield result
        }

      case ExecutionPlan.TupleExecution(operation) =>
        operation match {
          case TupleOp.Arg(plan, i) =>
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
        }

      case ExecutionPlan.RecursiveExecution(operation) =>
        operation match {
          case RecursiveOp.RepeatWhile(f, cond) =>
            def loop(input: DynamicValue): Task[DynamicValue] = {
              for {
                output <- evalDynamic(f, input)
                isTrue <- eval[Boolean](cond, output)
                result <- if (isTrue) loop(output) else ZIO.succeed(output)
              } yield result
            }

            loop(input)

          case RecursiveOp.DoWhile(f, cond) =>
            def loop: Task[DynamicValue] = {
              for {
                output <- evalDynamic(f, input)
                isTrue <- eval[Boolean](cond, input)
                result <- if (isTrue) loop else ZIO.succeed(output)
              } yield result
            }

            loop
        }
      case ExecutionPlan.SourceExecution(operation)    =>
        operation match {
          case SourceOp.Default(value)  => ZIO.succeed(value)
          case SourceOp.FromMap(value)  =>
            ZIO.succeed(value.get(input) match {
              case Some(value) => DynamicValue.SomeValue(value)
              case None        => DynamicValue.NoneValue
            })
          case SourceOp.Constant(value) => ZIO.succeed(value)
        }

      case ExecutionPlan.OpticalExecution(operation) =>
        operation match {
          case OpticalOp.GetPath(path) =>
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

          case OpticalOp.SetPath(path) =>
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
        }

      case ExecutionPlan.LogicalExecution(operation) =>
        operation match {
          case LogicalOp.And(left, right) =>
            for {
              left  <- eval[Boolean](left, input)
              right <- eval[Boolean](right, input)
            } yield toDynamic {
              left && right
            }
          case LogicalOp.Or(left, right)  =>
            for {
              left  <- eval[Boolean](left, input)
              right <- eval[Boolean](right, input)
            } yield toDynamic {
              left || right
            }

          case LogicalOp.Not(plan) =>
            for {
              bool <- eval[Boolean](plan, input)
            } yield toDynamic(!bool)

          case LogicalOp.Equals(left, right) =>
            for {
              left  <- evalDynamic(left, input)
              right <- evalDynamic(right, input)
            } yield toDynamic(left == right)

          case LogicalOp.Diverge(cond, ifTrue, ifFalse) =>
            for {
              cond   <- eval[Boolean](cond, input)
              result <- if (cond) evalDynamic(ifTrue, input) else evalDynamic(ifFalse, input)
            } yield result
        }

      case ExecutionPlan.NumericExecution(operation, NumericOp.Kind.IntNumber) =>
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

      case ExecutionPlan.ArrowExecution(operation) =>
        operation match {
          case ArrowOp.Zip(left, right) =>
            for {
              a <- evalDynamic(left, input)
              b <- evalDynamic(right, input)
            } yield DynamicValue.Tuple(a, b)

          case ArrowOp.Pipe(first, second) =>
            for {
              input  <- evalDynamic(first, input)
              output <- evalDynamic(second, input)
            } yield output

          case ArrowOp.Identity => ZIO.succeed(input)
        }

    }
  }
}

object InMemoryInterpreter {
  def make: UIO[InMemoryInterpreter] =
    Scope.inMemory[ContextId, ScopeId, DynamicValue].map(scope => new InMemoryInterpreter(scope))

  private def toDynamic[A](a: A)(implicit schema: Schema[A]): DynamicValue =
    schema.toDynamic(a)
}
