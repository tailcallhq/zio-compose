package compose.graphql

import zio.test._
import compose.Lambda
import compose.graphql.GraphQLParser
import zio.test.Assertion._
import zio.test.TestAspect._

object GraphQLExecutorSpec extends ZIOSpecDefault {
  val thousand = Lambda.constant(1000)
  def spec     = suite("GraphQLExecutorSpec")(test("simple query") {

    // schema: type Root { thousand: Int }
    val edges  = Edge[Unit, Unit]("thousand", thousand)
    val query  = GraphQLParser.querySyntax.parseString("query { thousand }")
    val result = GraphQLExecutor.execute(edges, query)

    assertZIO(result)(equalTo(1000))
  }) @@ failing
}
