package compose

import caliban.GraphQL.graphQL
import caliban.RootResolver
import zio.Scope
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}

object GraphQLSchemaSpec extends ZIOSpecDefault {

  def !![A, B]: A ~> B = Lambda.unsafe.attempt[A, B](???)

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Endpoint")(
    suite("api.render")(
      test("render") {
        final case class Foo(
          a1: Any ~> Int = !!,
          a2: Int ~> Int = !!,
          a3: Option[String] ~> Option[Int] = !!,
          a4: (String, Int, Boolean) ~> Int = !!,
        )

        val foo = Foo(
          !![Any, Int],
          !![Int, Int],
          !![Option[String], Option[Int]],
          !![(String, Int, Boolean), Int],
        )
        val api = graphQL(RootResolver(foo))

        val query =
          """|schema {
             |  query: Foo
             |}
             |
             |type Foo {
             |  a1: Int!
             |  a2(value: Int!): Int!
             |  a3(value: String): Int
             |  a4(_1: String!, _2: Int!, _3: Boolean!): Int!
             |}
        """.stripMargin.trim

        assertTrue(api.render == query)
      },
    ),
  )
}
