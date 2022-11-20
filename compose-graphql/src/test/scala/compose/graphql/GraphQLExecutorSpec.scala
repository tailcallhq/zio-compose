package compose.graphql

import compose.Lambda
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test._

object GraphQLExecutorSpec extends ZIOSpecDefault {

  def spec = suite("GraphQLExecutorSpec")(test("simple query") {

    val thousand = Lambda.constant(1000)
    val edges    = Edge[Unit, Unit]("thousand", thousand)

    val query    = GraphQLParser.parse("query { thousand }")
    val expected = Json.Obj("thousand" -> Json.Num(1000))

    val program = GraphQLExecutor.execute(edges, query.getOrElse(null))

    assertZIO(program)(equalTo(expected))
  })
}
