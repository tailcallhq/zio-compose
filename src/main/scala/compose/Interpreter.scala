package compose

import zio.schema.{DynamicValue, Schema}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import zio.{Ref, Task, UIO, ZIO}
import zio.schema.codec.JsonCodec

final case class Interpreter(scope: Interpreter.Scope[Int, Int, DynamicValue]) {
  import Interpreter._

  def eval(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue] = {
    plan match {
      case ExecutionPlan.EndScope(id) =>
        scope.deleteScope(id).as(Schema[Unit].toDynamic {})

      case ExecutionPlan.Debug(name) =>
        val json = new String(JsonCodec.encode(Schema[DynamicValue])(input).toArray)
        ZIO.succeed(println(s"${name}: $json")).as(input)

      case ExecutionPlan.Arg(i, a1, a2) =>
        val s1 = a1.toSchema.asInstanceOf[Schema[Any]]
        val s2 = a2.toSchema.asInstanceOf[Schema[Any]]

        for {
          value  <- effect(input.toTypedValue(Schema.tuple2(s1, s2)))
          result <- i match {
            case 0 => ZIO.succeed(encode(value._1)(s1))
            case 1 => ZIO.succeed(encode(value._2)(s2))
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
        } yield encode(())

      case ExecutionPlan.RepeatWhile(f, cond) =>
        def loop(input: DynamicValue): Task[DynamicValue] = {
          for {
            output <- eval(f, input)
            isTrue <- evalTyped[Boolean](cond, output)
            result <- if (isTrue) loop(output) else ZIO.succeed(output)
          } yield result
        }

        loop(input)

      case ExecutionPlan.DoWhile(f, cond) =>
        def loop: Task[DynamicValue] = {
          for {
            output <- eval(f, input)
            isTrue <- evalTyped[Boolean](cond, input)
            result <- if (isTrue) loop else ZIO.succeed(output)
          } yield result
        }

        loop

      case ExecutionPlan.Concat(self, other, canConcat) =>
        for {
          canConcat <- effect(canConcat.toTypedValue(Schema[CanConcat[_]]))
          result    <- canConcat match {
            case CanConcat.ConcatString =>
              evalTyped[String](self, input).zipWithPar(evalTyped[String](other, input)) { case (a, b) =>
                encode(a + b)
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
          left  <- evalTyped[Boolean](left, input)
          right <- evalTyped[Boolean](right, input)
        } yield encode { left && right }

      case ExecutionPlan.LogicalOr(left, right) =>
        for {
          left  <- evalTyped[Boolean](left, input)
          right <- evalTyped[Boolean](right, input)
        } yield encode { left || right }

      case ExecutionPlan.LogicalNot(plan)                             =>
        for {
          bool <- evalTyped[Boolean](plan, input)
        } yield encode { !bool }
      case ExecutionPlan.NumericOperation(operation, left, right, is) =>
        for {
          isNumeric <- effect(is.toTypedValue(Schema[IsNumeric[_]]))
          params    <-
            isNumeric match {
              case IsNumeric.NumericInt =>
                evalTyped[Int](left, input).zip(evalTyped[Int](right, input)).map { case (a, b) =>
                  operation match {
                    case Numeric.Operation.Add                => a + b
                    case Numeric.Operation.Multiply           => a * b
                    case Numeric.Operation.Subtract           => a - b
                    case Numeric.Operation.Divide             => a / b
                    case Numeric.Operation.GreaterThan        => if (a > b) 1 else 0
                    case Numeric.Operation.GreaterThanEqualTo => if (a >= b) 1 else 0
                  }
                }
            }
        } yield encode(params)

      case ExecutionPlan.Zip(left, right) =>
        for {
          a <- eval(left, input)
          b <- eval(right, input)
        } yield DynamicValue.Tuple(a, b)

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
          left  <- eval(left, input)
          right <- eval(right, input)
        } yield encode(left == right)

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

  def evalDynamic(plan: ExecutionPlan, value: DynamicValue): Task[DynamicValue] =
    eval(plan, value)

  def evalTyped[A](plan: ExecutionPlan, value: DynamicValue)(implicit ev: Schema[A]): Task[A] =
    evalDynamic(plan, value).flatMap(value => effect(value.toTypedValue(ev)))
}
object Interpreter                                                             {

  def evalDynamic(plan: ExecutionPlan, value: DynamicValue): Task[DynamicValue] =
    make.flatMap(i => i.evalDynamic(plan, value))

  def evalDynamic[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[DynamicValue] =
    evalDynamic(lmb.compile, Schema.primitive[Unit].toDynamic(()))

  def evalTyped[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[B] =
    evalTyped[B](lmb.compile, Schema.primitive[Unit].toDynamic(()))

  def evalTyped[A](plan: ExecutionPlan, value: DynamicValue)(implicit ev: Schema[A]): Task[A] =
    make.flatMap(i => i.evalTyped(plan, value))

  def make: UIO[Interpreter] =
    Scope.make[Int, Int, DynamicValue].map(scope => new Interpreter(scope))

  private def effect[A](e: Either[String, A])(implicit trace: zio.Trace): Task[A] =
    e match {
      case Left(error) => ZIO.fail(new Exception(error))
      case Right(a)    => ZIO.succeed(a)
    }

  private def encode[A](a: A)(implicit schema: Schema[A]): DynamicValue =
    schema.toDynamic(a)

  final case class Scope[S, K, V](ref: Ref[Map[(S, K), V]]) {
    def deleteScope(scope: S): UIO[Unit] = ref.update { map => map.filter { case s -> _ -> _ => s != scope } }

    def get(scope: S, key: K): UIO[Option[V]] = ref.get.map(_.get(scope, key))

    def set(scope: S, key: K, value: V): UIO[Unit] = ref.update { map => map + (((scope, key), value)) }
  }

  object Scope {
    def make[S, K, V]: UIO[Scope[S, K, V]] = Ref.make(Map.empty[(S, K), V]).map(Scope(_))
  }
}
