package compose.graphql

import caliban.GraphQL.graphQL
import caliban.RootResolver
import compose.{Lambda, ~>}
import zio.Scope
import zio.schema.{DeriveSchema, Schema}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}

object GraphQLSchemaSpec extends ZIOSpecDefault {

  import GraphQLSchema.Implicits._

  def !![A, B]: A ~> B = Lambda.unsafe.attempt[A, B](???)

  final case class Bar(a1: String, a2: Int)

  object Bar {
    implicit val schema: Schema[Bar] = DeriveSchema.gen[Bar]
  }

  final case class Foo(
    a0: Int = 0,
    a1: Any ~> Int = !!,
    a2: Int ~> Int = !!,
    a3: Option[String] ~> Option[Int] = !!,
    a4: (String, Int, Boolean) ~> Int = !!,
    a5: Bar ~> Int = !!,
    a6: Bar ~> Bar = !!,
  )

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Endpoint")(
    suite("api.render")(
      test("render") {

        val api = graphQL(RootResolver(Foo()))

        val query =
          """|schema {
             |  query: Foo
             |}
             |
             |type Bar {
             |  a1: String!
             |  a2: Int!
             |}
             |
             |type Foo {
             |  a0: Int!
             |  a1: Int!
             |  a2(value: Int!): Int!
             |  a3(value: String): Int
             |  a4(_1: String!, _2: Int!, _3: Boolean!): Int!
             |  a5(a1: String!, a2: Int!): Int!
             |  a6(a1: String!, a2: Int!): Bar!
             |}
        """.stripMargin.trim

        assertTrue(api.render == query)
      },
    ),
  )
}