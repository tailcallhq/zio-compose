package compose.internal

import caliban.introspection.adt.__Type
import caliban.schema.Step
import compose.~>
import zio.query.ZQuery
import zio.schema.Schema

object GraphQLSchema {

  type CalibanSchema[R, A] = caliban.schema.Schema[R, A]
  val CalibanSchema: caliban.schema.Schema.type = caliban.schema.Schema

  trait Implicits {
    implicit def lambdaSchema[R, A](implicit schemaA: Schema[A], ev: CalibanSchema[R, A]): CalibanSchema[R, Any ~> A] =
      new CalibanSchema[R, Any ~> A] {
        override protected[this] def toType(isInput: Boolean, isSubscription: Boolean): __Type =
          ev.toType_(isInput, isSubscription)

        override def resolve(value: Any ~> A): Step[R] =
          Step.QueryStep(ZQuery.fromZIO(value.eval(()).map(ev.resolve)))
      }
  }
}
