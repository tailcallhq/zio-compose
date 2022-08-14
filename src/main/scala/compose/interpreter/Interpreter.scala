package compose.interpreter

import zio.{Task, UIO, ZIO}
import zio.schema.{DynamicValue, Schema}
import compose.{~>, ExecutionPlan}

trait Interpreter {
  def eval(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue]

  def evalDynamic[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[DynamicValue] =
    evalDynamic(lmb.compile, Schema.primitive[Unit].toDynamic(()))

  def evalDynamic(plan: ExecutionPlan, value: DynamicValue): Task[DynamicValue] =
    eval(plan, value)

  def evalTyped[B](lmb: Any ~> B)(implicit b: Schema[B]): Task[B] =
    evalTyped[B](lmb.compile, Schema.primitive[Unit].toDynamic(()))

  def evalTyped[A](plan: ExecutionPlan, value: DynamicValue)(implicit ev: Schema[A]): Task[A] =
    evalDynamic(plan, value).flatMap(value => Interpreter.effect(value.toTypedValue(ev)))
}

object Interpreter {
  def effect[E, A](e: Either[String, A])(implicit trace: zio.Trace): Task[A] =
    e match {
      case Left(error) => ZIO.fail(new Exception(error))
      case Right(a)    => ZIO.succeed(a)
    }

  def inMemory: UIO[Interpreter] = InMemory.make
}
