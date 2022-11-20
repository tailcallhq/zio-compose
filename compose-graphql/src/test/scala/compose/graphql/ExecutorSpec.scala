package compose.graphql

import compose.Lambda
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test._

object ExecutorSpec extends ZIOSpecDefault {

  def spec = suite("GraphQLExecutorSpec")(test("simple query") {

    val thousand = Lambda.constant(1000)
    val edges    = Graph[Unit, Unit]("thousand", thousand)

    val query    = GraphQLParser.parse("query { thousand }")
    val expected = Json.Obj("thousand" -> Json.Num(1000))

    val program = Executor.execute(edges, query.getOrElse(null))

    assertZIO(program)(equalTo(expected))
  })
}