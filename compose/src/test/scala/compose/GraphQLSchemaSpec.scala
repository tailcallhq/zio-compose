package compose

import caliban.GraphQL.graphQL
import caliban.RootResolver
import zio.Scope
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}

object GraphQLSchemaSpec extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Endpoint")(
    suite("api.render")(
      test("Any ~> Int") {
        case class Foo(foo: Any ~> Int)
        val length = Foo(Lambda.constant(1))
        val api    = graphQL(RootResolver(length))

        val query =
          """|schema {
             |  query: Foo
             |}
             |
             |type Foo {
             |  foo: Int!
             |}
        """.stripMargin.trim

        assertTrue(api.render == query)
      },
      test("Int ~> Int") {
        case class Foo(foo: Int ~> Int)
        val length = Foo(Lambda.constant(1).inc)
        val api    = graphQL(RootResolver(length))

        val query =
          """|schema {
             |  query: Foo
             |}
             |
             |type Foo {
             |  foo(value: Int!): Int!
             |}
        """.stripMargin.trim

        assertTrue(api.render == query)
      },
    ),
  )
}
