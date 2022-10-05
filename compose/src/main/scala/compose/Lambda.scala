package compose

import compose.ExecutionPlan._
import compose.dsl._
import compose.internal.GraphQLSchema
import compose.model.Transformation
import zio.schema.Schema

trait Lambda[-A, +B]
    extends ArrowDSL.Op[A, B]
    with NumericDSL.Op[A, B]
    with TupleDSL.Op[A, B]
    with BooleanDSL.Op[A, B]
    with StringDSL.Op[A, B]
    with FoldDSL.Op[A, B]
    with OptionDSL.Op[A, B]
    with EitherDSL.Op[A, B]
    with LoopDSL.Op[A, B]
    with DebugDSL.Op[A, B]
    with CodecDSL.Op[A, B]
    with ListDSL.Op[A, B] { self =>

  final def ->>[I >: B, C](other: (C, I) ~> C): Transformation[A, C] =
    self transform other

  def compile: ExecutionPlan

  final def narrow[A1](implicit ev: A1 <:< A): A1 ~> B = self.asInstanceOf[A1 ~> B]

  final def widen[B1](implicit ev: B <:< B1): A ~> B1 = self.asInstanceOf[A ~> B1]
}

object Lambda
    extends ScopeDSL.Ctr
    with ConsoleDSL.Ctr
    with FoldDSL.Implicits
    with StringDSL.Implicits
    with RandomDSL.Ctr
    with RemoteDSL.Ctr
    with GraphQLSchema.Implicits {

  def constant[B](b: B)(implicit schema: Schema[B]): Any ~> B =
    Lambda.unsafe.attempt[Any, B] { Sources.Constant(schema.toDynamic(b)) }

  def default[A](implicit schema: Schema[A]): Any ~> A = Lambda.unsafe.attempt[Any, A] {
    Sources
      .Default(schema.defaultValue match {
        case Left(cause)  => throw new Exception(cause)
        case Right(value) => schema.toDynamic(value)
      })
  }

  def die: Any ~> Nothing = Lambda.unsafe.attempt[Any, Nothing] { Sources.Die }

  def fromMap[A, B](source: Map[A, B])(implicit input: Schema[A], output: Schema[B]): Lambda[A, Option[B]] =
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

  object unsafe {
    trait Attempt[A, B] {
      def apply(plan: => ExecutionPlan): A ~> B = new ~>[A, B] {
        override def compile: ExecutionPlan = plan
      }
    }

    object attempt {
      def apply[A, B]: Attempt[A, B] = new Attempt[A, B] {}
    }
  }

  implicit def schema[A, B]: Schema[A ~> B] = Schema[ExecutionPlan].transform[A ~> B](_.toLambda[A, B], _.compile)
}
