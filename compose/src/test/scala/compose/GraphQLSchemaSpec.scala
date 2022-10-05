package compose

import caliban.GraphQL.graphQL
import caliban.RootResolver
import zio.Scope
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}

object GraphQLSchemaSpec extends ZIOSpecDefault {
  case class Length(length: Any ~> Int)

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Endpoint")(
    test("Any ~> Int") {
      val length = Length(Lambda.constant(1))
      val api    = graphQL(RootResolver(length))

      val query = """|schema {
                     |  query: Length
                     |}
                     |
                     |type Length {
                     |  length: Int!
                     |}
        """.stripMargin.trim

      assertTrue(api.render == query)
    },
  )
}
