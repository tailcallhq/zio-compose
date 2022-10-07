package compose.graphql

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.introspection.adt.{__InputValue, __Type, __TypeKind}
import caliban.schema.Step.{FunctionStep, QueryStep}
import caliban.schema.{ArgBuilder, Step}
import compose.~>
import zio.query.ZQuery
import zio.schema.Schema

object GraphQLSchema {

  type CalibanSchema[-R, A] = caliban.schema.Schema[R, A]
  val CalibanSchema: caliban.schema.Schema.type = caliban.schema.Schema
  object Implicits {
    implicit def any2A[R, B](implicit schemaB: Schema[B], ev: CalibanSchema[R, B]): CalibanSchema[R, Any ~> B] =
      new CalibanSchema[R, Any ~> B] {
        override def optional: Boolean = ev.optional

        override protected[this] def toType(isInput: Boolean, isSubscription: Boolean): __Type =
          ev.toType_(isInput, isSubscription)

        override def resolve(value: Any ~> B): Step[R] =
          Step.QueryStep(ZQuery.fromZIO(value.eval(()).map(ev.resolve)))
      }

    implicit def a2b[R, A, B](implicit
      schemaA: Schema[A],
      argBuilder: ArgBuilder[A],
      cSchemaA: CalibanSchema[R, Any ~> A],
      cSchemaB: CalibanSchema[R, Any ~> B],
    ): CalibanSchema[R, A ~> B] =
      new CalibanSchema[R, A ~> B] {
        private lazy val inputType        = cSchemaA.toType_(true)
        private val unwrappedArgumentName = "value"

        override def arguments: List[__InputValue] = {
          inputType.inputFields.getOrElse(
            handleInput(List.empty[__InputValue])(
              List(
                __InputValue(
                  unwrappedArgumentName,
                  None,
                  () => if (cSchemaA.optional) inputType else inputType.nonNull,
                  None,
                ),
              ),
            ),
          )
        }

        private def handleInput[T](onWrapped: => T)(onUnwrapped: => T): T =
          inputType.kind match {
            case __TypeKind.SCALAR | __TypeKind.ENUM | __TypeKind.LIST =>
              // argument was not wrapped in a case class
              onUnwrapped
            case _                                                     => onWrapped
          }

        override protected[this] def toType(isInput: Boolean, isSubscription: Boolean): __Type =
          cSchemaB.toType_(isInput, isSubscription)

        override def resolve(value: A ~> B): Step[R] = {
          FunctionStep { args =>
            val builder = argBuilder.build(InputValue.ObjectValue(args))
            handleInput(builder)(
              builder.fold(
                error => args.get(unwrappedArgumentName).fold[Either[ExecutionError, A]](Left(error))(argBuilder.build),
                Right(_),
              ),
            ).fold(
              error => QueryStep(ZQuery.fail(error)),
              result => cSchemaB.resolve(value.bind(result)),
            )
          }
        }

        override def optional: Boolean = cSchemaB.optional
      }
  }
}
