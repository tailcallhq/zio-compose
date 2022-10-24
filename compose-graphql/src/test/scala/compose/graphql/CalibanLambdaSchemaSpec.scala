package compose.graphql

import caliban.GraphQL.graphQL
import caliban.RootResolver
import compose.{Lambda, ~>}
import zio.Scope
import zio.schema.{DeriveSchema, Schema}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}

object CalibanLambdaSchemaSpec extends ZIOSpecDefault {

  import CalibanLambdaSchema.Implicits._

  final case class Bar(a1: String, a2: Int)

  object Bar {
    implicit val schema: Schema[Bar] = DeriveSchema.gen[Bar]
  }

  final case class Foo(
    a0: Int = 0,
    a1: Any ~> Int = Lambda.die,
    a2: Int ~> Int = Lambda.die,
    a3: Option[String] ~> Option[Int] = Lambda.die,
    a4: (String, Int, Boolean) ~> Int = Lambda.die,
    a5: Bar ~> Int = Lambda.die,
    a6: Bar ~> Bar = Lambda.die,
  )

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Endpoint")(suite("api.render")(test("render") {

      val api = graphQL(RootResolver(Foo()))

      val query = """|schema {
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
    }))
}
