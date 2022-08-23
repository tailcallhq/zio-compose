package compose

import compose.dsl._
import compose.lens.Transformation
import ExecutionPlan._
import compose.Lambda.make
import zio.schema.Schema

trait Lambda[-A, +B]
    extends ArrowDSL[A, B]
    with NumericDSL[A, B]
    with TupleDSL[A, B]
    with BooleanDSL[A, B]
    with StringDSL[A, B]
    with FoldDSL[A, B]
    with OptionDSL[A, B]
    with EitherDSL[A, B]
    with LoopDSL[A, B] { self =>

  final def ->>[I >: B, C](other: (C, I) ~> C): Transformation[A, C] =
    self transform other

  def compile: ExecutionPlan

  final def debug[B1 >: B](name: String): A ~> B1 = {
    make[A, B1] { Debugger.Debug(self.compile, name) }
  }

  final def narrow[A1](implicit ev: A1 <:< A): A1 ~> B = self.asInstanceOf[A1 ~> B]

  final def widen[B1](implicit ev: B <:< B1): A ~> B1 = self.asInstanceOf[A ~> B1]
}

object Lambda extends ScopeDSL with ConsoleDSL with FoldDSL.Implicits with StringDSL.Implicits {

  def constant[B](b: B)(implicit schema: Schema[B]): Any ~> B =
    make[Any, B] { Sources.Constant(schema.toDynamic(b)) }

  def default[A](implicit schema: Schema[A]): Any ~> A = make[Any, A] {
    Sources
      .Default(schema.defaultValue match {
        case Left(cause)  => throw new Exception(cause)
        case Right(value) => schema.toDynamic(value)
      })
  }

  def fromMap[A, B](
    source: Map[A, B],
  )(implicit input: Schema[A], output: Schema[B]): Lambda[A, Option[B]] =
    Lambda.make[A, Option[B]](
      Sources.FromMap(source.map { case (a, b) => (input.toDynamic(a), output.toDynamic(b)) }),
    )

  def identity[A]: Lambda[A, A] = make[A, A] { Arrow.Identity }

  def id[A]: Lambda[A, A] = identity[A]

  def stats[A, B](f: A ~> B*): A ~> B = f.reduce(_ *> _)

  def transform[A, B](transformations: Transformation[A, B]*)(implicit s: Schema[B]): A ~> B =
    transformations.foldLeft[A ~> B](default[B]) {
      case (ab, transformation: Transformation.Constructor[A @unchecked, B @unchecked, Any @unchecked]) =>
        transformation(ab)
    }

  trait UnsafeMake[A, B] {
    def apply(plan: => ExecutionPlan): A ~> B = new ~>[A, B] {
      override def compile: ExecutionPlan = plan
    }
  }

  private[compose] object make {
    def apply[A, B]: UnsafeMake[A, B] = new UnsafeMake[A, B] {}
  }
}
