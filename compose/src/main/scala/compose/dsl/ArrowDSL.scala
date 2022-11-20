package compose.dsl

import compose.ExecutionPlan._
import compose.Lambda.constant
import compose.model.Transformation
import compose.{Interpreter, Lambda, model, ~>}
import zio.Task
import zio.schema.Schema

object ArrowDSL {
  trait Op[-A, +B] {
    self: A ~> B =>
    final def =!=[A1 <: A, B1 >: B, B2](other: A1 ~> B2)(implicit ev: B1 =:= B2): A1 ~> Boolean =
      self notEq other

    final def =:=[A1 <: A, B1 >: B, B2](other: A1 ~> B2)(implicit ev: B1 =:= B2): A1 ~> Boolean =
      self eq other

    final def >>>[C](other: B ~> C): A ~> C = self pipe other

    final def <<<[X](other: X ~> A): X ~> B = self compose other

    final def <*[A1 <: A, B1 >: B, B2](other: A1 ~> B2): A1 ~> B1 = (self: A1 ~> B1) zipLeft other

    final def <*>[A1 <: A, B1 >: B, B2](other: A1 ~> B2): A1 ~> (B1, B2) =
      (self: A1 ~> B1) zip other

    final def *>[A1 <: A, B1 >: B, B2](other: A1 ~> B2): A1 ~> B2 = (self: A1 ~> B1) zipRight other

    final def as[C](c: C)(implicit s: Schema[C]): A ~> C = self >>> constant(c)

    final def asString[B1 >: B](implicit b: Schema[B1]): A ~> String = self >>> Lambda.unsafe
      .attempt[B, String] { Arrow.AsString(b) }

    final def bind[A1 <: A](a: A1)(implicit ev: Schema[A1]): Any ~> B = Lambda.constant(a) >>> self

    final def compose[X](other: X ~> A): X ~> B = other pipe self

    final def eval[A1 <: A, B1 >: B](a: A1)(implicit in: Schema[A1], out: Schema[B1]): Task[B1] =
      Interpreter.inMemory.flatMap(_.eval[B1](self.compile, in.toDynamic(a)))

    final def notEq[A1 <: A, B1 >: B](other: A1 ~> B1): A1 ~> Boolean = (self eq other).not

    final def pipe[C](other: B ~> C): A ~> C = Lambda.unsafe
      .attempt[A, C] { Arrow.Pipe(self.compile, other.compile) }

    final def toInt: A ~> Either[String, Int] = self >>> Lambda.unsafe
      .attempt[Any, Either[String, Int]](Arrow.ToInt)

    final def transform[I >: B, C](other: (C, I) ~> C): Transformation[A, C] = model
      .Transformation[A, C, I](self, other)

    final def unit: A ~> Unit = self.as(())

    final def zip[A1 <: A, B1 >: B, B2](other: A1 ~> B2): A1 ~> (B1, B2) = Lambda.unsafe
      .attempt[A1, (B1, B2)] { Arrow.Zip(self.compile, other.compile) }

    final def zipLeft[A1 <: A, B1 >: B, B2](other: A1 ~> B2): A1 ~> B1 =
      ((self: A1 ~> B1) <*> other)._1

    final def zipRight[A1 <: A, B1 >: B, B2](other: A1 ~> B2): A1 ~> B2 =
      ((self: A1 ~> B1) <*> other)._2
  }
}
