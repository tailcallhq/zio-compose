package compose

import compose.internal.netty.HttpClient
import compose.model.{Decoder, Ref, Scope}
import compose.model.Decoder.HasDecoder
import compose.model.http.{Request, Response}
import zio.schema.codec.JsonCodec
import zio.schema.{DynamicValue, Schema}
import zio.{Chunk, Task, UIO, ZIO}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

trait Interpreter {
  def eval[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[B] =
    eval[B](lmb.compile, Schema.primitive[Unit].toDynamic(()))

  def eval[A](plan: ExecutionPlan, value: DynamicValue)(implicit ev: Schema[A]): Task[A] =
    evalDynamic(plan, value).flatMap(value => Interpreter.effect(value.toTypedValue(ev)))

  def evalDynamic[B](lmb: Any ~> B): Task[DynamicValue] =
    evalDynamic(lmb.compile, Schema.primitive[Unit].toDynamic(()))

  def evalDynamic(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue]

  def evalJson[B](lmb: Any ~> B): Task[String] =
    evalDynamic(lmb).map(res => new String(JsonCodec.encode(Schema[DynamicValue])(res).toArray))
}

object Interpreter {
  def effect[E, A](e: => Either[String, A]): Task[A] = {
    ZIO.suspendSucceed {
      e match {
        case Left(error) => ZIO.fail(new Exception(error))
        case Right(a)    => ZIO.succeed(a)
      }
    }
  }

  def eval[B](f: Any ~> B)(implicit ev: Schema[B]): Task[B] =
    for {
      int <- Interpreter.inMemory
      res <- int.eval(f)
    } yield res

  def inMemory: UIO[Interpreter] = for {
    scope <- ScopeContext.inMemory[Scope, Ref.Id, DynamicValue]
    http  <- HttpClient.make
  } yield new InMemoryInterpreter(scope, http)

  final class InMemoryInterpreter(scope: ScopeContext[Scope, Ref.Id, DynamicValue], client: HttpClient)
      extends Interpreter {
    import ExecutionPlan._

    def evalDynamic(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue] = {
      plan match {
        case operation: Debugger      => debugger(input, operation)
        case operation: Textual       => textual(input, operation)
        case operation: Scoped        => scoped(input, operation)
        case operation: Tupled        => tupled(input, operation)
        case operation: Recursive     => recursive(input, operation)
        case operation: Sources       => sources(input, operation)
        case operation: Optical       => optical(input, operation)
        case operation: Logical       => logical(input, operation)
        case Numeric(operation, kind) => numeric(input, operation, kind)
        case operation: Arrow         => arrowed(input, operation)
        case operation: Console       => console(input, operation)
        case operation: Fold          => fold(input, operation)
        case operation: Optional      => optional(input, operation)
        case operation: EitherOne     => eitherOne(input, operation)
        case operation: Random        => random(input, operation)
        case operation: Codec         => codec(input, operation)
        case operation: Remote        => remote(input, operation)
      }
    }

    private def codec(input: DynamicValue, operation: Codec): Task[DynamicValue] =
      operation match {
        case Codec.Encode               => ZIO.succeed(Schema[DynamicValue].toDynamic(input))
        case Codec.Decode(ast, decoder) =>
          for {
            dc <- effect(decoder.toTypedValue(Schema[Decoder]))
          } yield dc match {
            case decoder: Decoder.HasDecoder[_] =>
              decoder match {
                case HasDecoder.ResponseDecoder     =>
                  val schema    = ast.toSchema.asInstanceOf[Schema[Any]]
                  val jsonCodec = JsonCodec.decode(schema)
                  val value     = input.toTypedValue(Schema[Response]).flatMap(res => jsonCodec(res.body))
                  Schema.either(Schema[String], schema).toDynamic(value)
                case HasDecoder.DynamicValueDecoder =>
                  val schema = ast.toSchema.asInstanceOf[Schema[Any]]
                  val value  = input.toTypedValue(Schema[DynamicValue]).flatMap(dv => dv.toTypedValue(schema))
                  Schema.either(Schema[String], schema).toDynamic(value)
              }
          }
      }

    private def remote(input: DynamicValue, operation: Remote): Task[DynamicValue] =
      operation match {
        case Remote.Http =>
          for {
            req <- effect(input.toTypedValue(Schema[Request]))
            res <- ZIO.async[Any, Nothing, Response] { cb =>
              client.request(req.method, req.url, req.headers, req.body) { (status, headers, body) =>
                cb(ZIO.succeed(Response(status, headers, Chunk.fromArray(body))))
              }
            }
          } yield Schema.toDynamic(res)
      }

    private def random(input: DynamicValue, operation: Random): Task[DynamicValue] = {
      operation match {
        case Random.NextInt(min, max) =>
          for {
            min <- eval[Int](min, input)
            max <- eval[Int](max, input)
            rnd <- zio.Random.nextIntBetween(min, max)
          } yield toDynamic(rnd)
      }
    }

    private def eitherOne(input: DynamicValue, operation: EitherOne): Task[DynamicValue] =
      operation match {
        case EitherOne.IsLeft =>
          input match {
            case _: DynamicValue.LeftValue  => ZIO.succeed(toDynamic(true))
            case _: DynamicValue.RightValue => ZIO.succeed(toDynamic(false))
            case input                      => ZIO.fail(new RuntimeException(s"Not an either value: ${input}"))
          }
      }

    private def optional(input: DynamicValue, operation: Optional): Task[DynamicValue] =
      operation match {
        case Optional.IsEmpty =>
          input match {
            case DynamicValue.NoneValue    => ZIO.succeed(toDynamic(true))
            case DynamicValue.SomeValue(_) => ZIO.succeed(toDynamic(false))
            case input                     => ZIO.fail(new RuntimeException(s"Not an optional value: ${input}"))
          }
      }

    private def arrowed(input: DynamicValue, operation: Arrow): Task[DynamicValue] = {
      operation match {
        case Arrow.Zip(left, right) =>
          for {
            a <- evalDynamic(left, input)
            b <- evalDynamic(right, input)
          } yield DynamicValue.Tuple(a, b)

        case Arrow.Pipe(first, second) =>
          for {
            input  <- evalDynamic(first, input)
            output <- evalDynamic(second, input)
          } yield output

        case Arrow.AsString(ast) =>
          effect(input.toTypedValue(ast.toSchema).map(any => toDynamic(any.toString)))

        case Arrow.Identity => ZIO.succeed(input)

        case Arrow.ToInt =>
          (for {
            result <- input match {
              case DynamicValue.Primitive(value, _) =>
                ZIO.attempt(value.toString.toInt).orElseFail("Cannot convert to Int")
              case _ => ZIO.fail("Cannot convert to int because input is not a primitive")
            }
          } yield result).either.map(toDynamic(_))
      }
    }

    private def console(input: DynamicValue, operation: Console): Task[DynamicValue] =
      operation match {
        case Console.WriteLine        =>
          for {
            string <- Interpreter.effect(input.toTypedValue(Schema[String]))
            _      <- zio.Console.printLine(string)
          } yield unit
        case Console.ReadLine(option) =>
          for {
            string <- option match {
              case Some(prompt) => zio.Console.readLine(prompt)
              case None         => zio.Console.readLine
            }
          } yield toDynamic(string)
      }

    private def debugger(input: DynamicValue, operation: Debugger): Task[DynamicValue] = {
      operation match {
        case Debugger.Debug(plan, name) =>
          for {
            result <- evalDynamic(plan, input)
            json = new String(JsonCodec.encode(Schema[DynamicValue])(result).toArray)
            _ <- zio.Console.printLine(s"${name}: $json")
          } yield result
        case Debugger.Show(plan, name)  =>
          val json = plan.json
          zio.Console.printLine(s"${name}: $json") *> evalDynamic(plan, input)

        case Debugger.Address(plan) => ZIO.succeed(toDynamic(plan.binary.hashCode().toHexString))
      }
    }

    private def fold(input: DynamicValue, operation: Fold): Task[DynamicValue] =
      operation match {
        case Fold.FoldOption(isEmpty, f) =>
          for {
            result <- input match {
              case DynamicValue.SomeValue(input) => evalDynamic(f, input)
              case DynamicValue.NoneValue        => evalDynamic(isEmpty, input)
              case _                             => ZIO.fail(new RuntimeException("Cannot fold on this input"))
            }
          } yield result

        case Fold.FoldEither(left, right) =>
          for {
            result <- input match {
              case DynamicValue.LeftValue(input)  => evalDynamic(left, input)
              case DynamicValue.RightValue(input) => evalDynamic(right, input)
              case _                              => ZIO.fail(new RuntimeException("Cannot fold on this input"))
            }
          } yield result

        case Fold.FoldList(ast, seed, fun) =>
          val schema = ast.toSchema.asInstanceOf[Schema[Any]]
          for {
            seed <- evalDynamic(seed, input)
            list <- effect(input.toTypedValue(Schema.list(schema)))
            res  <- ZIO.foldLeft(list)(seed) { (s, i) =>
              val di = Schema.toDynamic(i)(schema)
              evalDynamic(fun, DynamicValue.Tuple(s, di))
            }
          } yield res
      }

    private def logical(input: DynamicValue, operation: Logical): Task[DynamicValue] = {
      operation match {
        case Logical.And(left, right) =>
          for {
            left  <- eval[Boolean](left, input)
            right <- eval[Boolean](right, input)
          } yield toDynamic {
            left && right
          }
        case Logical.Or(left, right)  =>
          for {
            left  <- eval[Boolean](left, input)
            right <- eval[Boolean](right, input)
          } yield toDynamic {
            left || right
          }

        case Logical.Not(plan) =>
          for {
            bool <- eval[Boolean](plan, input)
          } yield toDynamic(!bool)

        case Logical.Equals(left, right) =>
          for {
            left  <- evalDynamic(left, input)
            right <- evalDynamic(right, input)
          } yield toDynamic(left == right)

        case Logical.Diverge(cond, ifTrue, ifFalse) =>
          for {
            cond   <- eval[Boolean](cond, input)
            result <- if (cond) evalDynamic(ifTrue, input) else evalDynamic(ifFalse, input)
          } yield result
      }
    }

    private def numeric(
      input: DynamicValue,
      operation: Numeric.Operation,
      kind: Numeric.Kind,
    ): Task[DynamicValue] = {
      kind match {
        case Numeric.Kind.IntNumber =>
          operation match {
            case Numeric.Add(left, right)                =>
              for {
                a <- eval[Int](left, input)
                b <- eval[Int](right, input)
              } yield toDynamic(a + b)
            case Numeric.Multiply(left, right)           =>
              for {
                a <- eval[Int](left, input)
                b <- eval[Int](right, input)
              } yield toDynamic(a * b)
            case Numeric.Divide(left, right)             =>
              for {
                a <- eval[Int](left, input)
                b <- eval[Int](right, input)
              } yield toDynamic(a / b)
            case Numeric.GreaterThan(left, right)        =>
              for {
                a <- eval[Int](left, input)
                b <- eval[Int](right, input)
              } yield toDynamic(a > b)
            case Numeric.GreaterThanEqualTo(left, right) =>
              for {
                a <- eval[Int](left, input)
                b <- eval[Int](right, input)
              } yield toDynamic(a >= b)
            case Numeric.Negate(plan)                    =>
              for {
                a <- eval[Int](plan, input)
              } yield toDynamic(-a)
          }
      }
    }

    private def optical(input: DynamicValue, operation: Optical): Task[DynamicValue] = {
      operation match {
        case Optical.GetPath(path) =>
          input match {
            case DynamicValue.Record(_, values) =>
              @tailrec
              def loop(
                path: List[String],
                values: ListMap[String, DynamicValue],
              ): Either[Exception, DynamicValue] = {
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
            case _                              => ZIO.fail(new Exception("Select only works on records"))
          }

        case Optical.SetPath(path) =>
          input match {
            case DynamicValue.Tuple(DynamicValue.Record(id, values), input) =>
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
                        if (tail.isEmpty) Right(DynamicValue.Record(id, values + (head -> a)))
                        else
                          loop(tail, v.asInstanceOf[DynamicValue.Record].values, a) map { value =>
                            DynamicValue.Record(id, values + (head -> value))
                          }
                    }
                }
              }

              ZIO.fromEither(loop(path, values, input))
            case input => ZIO.fail(new Exception(s"Set path doesn't work on: ${input}"))
          }
      }
    }

    private def recursive(input: DynamicValue, operation: Recursive): Task[DynamicValue] = {
      operation match {
        case Recursive.RecurseWhile(f, cond) =>
          def loop(input: DynamicValue): Task[DynamicValue] = {
            for {
              output <- evalDynamic(f, input)
              isTrue <- eval[Boolean](cond, output)
              result <- if (isTrue) loop(output) else ZIO.succeed(output)
            } yield result
          }

          loop(input)

        case Recursive.RepeatWhile(f, cond) =>
          def loop: Task[DynamicValue] = {
            for {
              output <- evalDynamic(f, input)
              isTrue <- eval[Boolean](cond, output)
              result <- if (isTrue) loop else ZIO.succeed(output)
            } yield result
          }

          loop
      }
    }

    private def scoped(input: DynamicValue, operation: Scoped): Task[DynamicValue] = {
      operation match {
        case Scoped.Set(refId, scopeId) =>
          for {
            _ <- scope.set(scopeId, refId, input)
          } yield toDynamic(())

        case Scoped.Get(refId, scopeId, value) =>
          for {
            option <- scope.get(scopeId, refId)
            value  <- option match {
              case Some(value) => ZIO.succeed(value)
              case None        => ZIO.succeed(value)
            }
          } yield value

        case Scoped.Using(plan, scopeId) =>
          for {
            result <- evalDynamic(plan, input)
            _      <- scope.delete(scopeId)
          } yield result
      }
    }

    private def sources(input: DynamicValue, operation: Sources): Task[DynamicValue] = {
      operation match {
        case Sources.Default(value)  => ZIO.succeed(value)
        case Sources.FromMap(value)  =>
          ZIO.succeed(value.get(input) match {
            case Some(value) => DynamicValue.SomeValue(value)
            case None        => DynamicValue.NoneValue
          })
        case Sources.Constant(value) => ZIO.succeed(value)
      }
    }

    private def textual(input: DynamicValue, operation: Textual): Task[DynamicValue] = {
      operation match {
        case Textual.Length(plan) =>
          for {
            str <- eval[String](plan, input)
          } yield toDynamic(str.length)

        case Textual.UpperCase(plan) =>
          for {
            str <- eval[String](plan, input)
          } yield toDynamic(str.toUpperCase)

        case Textual.LowerCase(plan) =>
          for {
            str <- eval[String](plan, input)
          } yield toDynamic(str.toLowerCase)

        case Textual.StartsWith(self, other) =>
          for {
            str1 <- eval[String](self, input)
            str2 <- eval[String](other, input)
          } yield toDynamic(str1.startsWith(str2))

        case Textual.EndsWith(self, other) =>
          for {
            str1 <- eval[String](self, input)
            str2 <- eval[String](other, input)
          } yield toDynamic(str1.endsWith(str2))

        case Textual.Contains(self, other) =>
          for {
            str1 <- eval[String](self, input)
            str2 <- eval[String](other, input)
          } yield toDynamic(str1.contains(str2))

        case Textual.Concat(self, other) =>
          for {
            str1 <- eval[String](self, input)
            str2 <- eval[String](other, input)
          } yield toDynamic(str1 ++ str2)
      }
    }

    private def toDynamic[A](a: A)(implicit schema: Schema[A]): DynamicValue =
      schema.toDynamic(a)

    private def tupled(input: DynamicValue, operation: Tupled): Task[DynamicValue] = {
      operation match {
        case Tupled.Arg(plan, i) =>
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
    }

    private def unit: DynamicValue = Schema.primitive[Unit].toDynamic(())
  }
}
