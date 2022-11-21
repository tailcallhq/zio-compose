package compose.graphql

import compose.Lambda
import compose.graphql.internal.JsonPlaceholder
import zio.test.Assertion._
import zio.test._

object ExecutorSpec extends ZIOSpecDefault {

  def spec = suite("GraphQLExecutorSpec")(
    test("simple query") {
      val graph = Graph[Unit, Unit]("thousand", Lambda.constant(1000))
      val query = "query { thousand }"

      val actual   = Executor.execute(edges, query.getOrElse(null)).map(_.toJson)
      val expected = """{"thousand":1000}"""

      assertZIO(actual)(equalTo(expected))
    },
    suite("JsonPlaceholder")(
      test("service call") {
        val edges = Graph[Unit, Unit]("users", JsonPlaceholder.fetch.users)
        val query = "query { users { id } }"

        val actual   = edges.execute(query).map(_.toJson)
        val expected = "{\"users\":[" +
          "{\"id\":1}," +
          "{\"id\":2}," +
          "{\"id\":3}," +
          "{\"id\":4}," +
          "{\"id\":5}," +
          "{\"id\":6}," +
          "{\"id\":7}," +
          "{\"id\":8}," +
          "{\"id\":9}," +
          "{\"id\":10}" +
          "]}"

        assertZIO(actual)(equalTo(expected))
      },
      test("nested query") {
        val graph = Graph[Unit, Unit]("users", JsonPlaceholder.fetch.users)
        val query = "query { users { id address { geo { lat } } } }"

        val actual   = graph.execute(query).map(_.toJson)
        val expected = "{\"users\":[" +
          "{\"id\":1,\"address\":{\"geo\":{\"lat\":\"-37.3159\"}}}," +
          "{\"id\":2,\"address\":{\"geo\":{\"lat\":\"-43.9509\"}}}," +
          "{\"id\":3,\"address\":{\"geo\":{\"lat\":\"-68.6102\"}}}," +
          "{\"id\":4,\"address\":{\"geo\":{\"lat\":\"29.4572\"}}}," +
          "{\"id\":5,\"address\":{\"geo\":{\"lat\":\"-31.8129\"}}}," +
          "{\"id\":6,\"address\":{\"geo\":{\"lat\":\"-71.4197\"}}}," +
          "{\"id\":7,\"address\":{\"geo\":{\"lat\":\"24.8918\"}}}," +
          "{\"id\":8,\"address\":{\"geo\":{\"lat\":\"-14.3990\"}}}," +
          "{\"id\":9,\"address\":{\"geo\":{\"lat\":\"24.6463\"}}}," +
          "{\"id\":10,\"address\":{\"geo\":{\"lat\":\"-38.2386\"}}}" +
          "]}"

        assertZIO(actual)(equalTo(expected))
      },
    ),
  )
}
