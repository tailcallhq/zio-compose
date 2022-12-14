package compose.graphql

import zio._
import caliban._
import caliban.schema._

object Executor {
  // TODO: support mutations and subscriptions
  def make(query: Graph): IO[CalibanError, GraphQL[Any]] = for {
    querySchema <- SchemaGenerator.gen(query)
    mutationSchema     = Schema.unitSchema
    subscriptionSchema = Schema.unitSchema
  } yield {
    GraphQL.graphQL(RootResolver(query), Nil, Nil)(
      SubscriptionSchema.unitSubscriptionSchema,
      querySchema,
      mutationSchema,
      subscriptionSchema,
    )
  }

}
