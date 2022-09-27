package compose

import compose.dsl._
import compose.model.Transformation
import ExecutionPlan._
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
    with LoopDSL[A, B]
    with DebugDSL[A, B]
    with CodecDSL[A, B] { self =>

  final def ->>[I >: B, C](other: (C, I) ~> C): Transformation[A, C] =
    self transform other

  def compile: ExecutionPlan

  final def narrow[A1](implicit ev: A1 <:< A): A1 ~> B = self.asInstanceOf[A1 ~> B]

  final def widen[B1](implicit ev: B <:< B1): A ~> B1 = self.asInstanceOf[A ~> B1]
}

object Lambda
    extends ScopeDSL
    with ConsoleDSL
    with FoldDSL.Implicits
    with StringDSL.Implicits
    with RandomDSL
    with RemoteDSL {

  def constant[B](b: B)(implicit schema: Schema[B]): Any ~> B =
    Lambda.unsafe.attempt[Any, B] { Sources.Constant(schema.toDynamic(b)) }

  def default[A](implicit schema: Schema[A]): Any ~> A = Lambda.unsafe.attempt[Any, A] {
    Sources
      .Default(schema.defaultValue match {
        case Left(cause)  => throw new Exception(cause)
        case Right(value) => schema.toDynamic(value)
      })
  }

  def fromMap[A, B](
    source: Map[A, B],
  )(implicit input: Schema[A], output: Schema[B]): Lambda[A, Option[B]] =
    Lambda.unsafe.attempt[A, Option[B]](
      Sources.FromMap(source.map { case (a, b) => (input.toDynamic(a), output.toDynamic(b)) }),
    )

  def id[A]: Lambda[A, A] = identity[A]

  def identity[A]: Lambda[A, A] = Lambda.unsafe.attempt[A, A] { Arrow.Identity }

  def seq[A, B](f: A ~> B*): A ~> B = f.reduce(_ *> _)

  def stats[A](f: A ~> Any*): A ~> Unit = seq(f: _*).unit

  def transform[A, B](transformations: Transformation[A, B]*)(implicit s: Schema[B]): A ~> B =
    transformations.foldLeft[A ~> B](default[B]) {
      case (ab, transformation: Transformation.Constructor[A @unchecked, B @unchecked, Any @unchecked]) =>
        transformation(ab)
    }

  def unit: Any ~> Unit = constant(())

  private[compose] object unsafe {
    trait Attempt[A, B] {
      def apply(plan: => ExecutionPlan): A ~> B = new ~>[A, B] {
        override def compile: ExecutionPlan = plan
      }
    }

    object attempt {
      def apply[A, B]: Attempt[A, B] = new Attempt[A, B] {}
    }
  }

}
