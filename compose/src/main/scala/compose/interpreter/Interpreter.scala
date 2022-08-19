package compose.interpreter

import zio.{Task, UIO, ZIO}
import zio.schema.{DynamicValue, Schema}
import compose.{~>, ExecutionPlan}
import zio.schema.codec.JsonCodec

trait Interpreter {
  def eval[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[B] =
    eval[B](lmb.compile, Schema.primitive[Unit].toDynamic(()))

  def eval[A](plan: ExecutionPlan, value: DynamicValue)(implicit ev: Schema[A]): Task[A] =
    evalDynamic(plan, value).flatMap(value => Interpreter.effect(value.toTypedValue(ev)))

  def evalDynamic[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[DynamicValue] =
    evalDynamic(lmb.compile, Schema.primitive[Unit].toDynamic(()))

  def evalDynamic(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue]

  def evalJson[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[String] =
    evalDynamic(lmb).map(res => new String(JsonCodec.encode(Schema[DynamicValue])(res).toArray))
}

object Interpreter {
  def effect[E, A](e: Either[String, A])(implicit trace: zio.Trace): Task[A] =
    e match {
      case Left(error) => ZIO.fail(new Exception(error))
      case Right(a)    => ZIO.succeed(a)
    }

  def eval[B](f: Any ~> B)(implicit ev: Schema[B]): Task[B] =
    for {
      int <- Interpreter.inMemory
      res <- int.eval(f)
    } yield res

  def inMemory: UIO[Interpreter] = InMemoryInterpreter.make
}
