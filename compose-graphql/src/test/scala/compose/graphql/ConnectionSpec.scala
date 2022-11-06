package compose.graphql

import compose.graphql.{AstGenerator, AstPrinter}
import compose.{Lambda, ~>}
import zio.schema.{DeriveSchema, Schema}
import zio.test.{ZIOSpecDefault, assertTrue}

object ConnectionSpec extends ZIOSpecDefault {

  def die[A, B]: A ~> B = Lambda.die

  final case class Scalars(a1: Int, a2: String, a3: Boolean, a4: Double)

  final case class OptionalSequences(
    a1: Option[List[Int]],
    a2: Option[List[Option[Int]]],
    a3: List[Option[Int]],
    a4: List[Option[List[Int]]],
  )

  final case class Optionals(
    a1: Option[Int],
    a2: Option[String],
    a3: Option[Boolean],
    a4: Option[Double],
    a5: Option[Option[Int]],
  )

  final case class Sequences(
    a1: List[Int],
    a2: List[String],
    a3: List[Boolean],
    a4: List[Double],
    a5: List[Scalars],
    a6: List[Optionals],
    a7: List[List[Int]],
  )

  final case class Root(
    optionals: Optionals,
    scalars: Scalars,
    sequences: Sequences,
    optionalSequences: OptionalSequences,
  )

  implicit val optionalSchema: Schema[Optionals] = DeriveSchema.gen[Optionals]
  implicit val scalarSchema: Schema[Scalars]     = DeriveSchema.gen[Scalars]
  implicit val schema: Schema[Root]              = DeriveSchema.gen[Root]

  def spec = suite("ConnectionSpec")(suite("schema")(test("render") {
    val connection = Connection.arg("root", die[Unit, Root])
    val graphQL    = AstGenerator.gen(Seq(connection))
    val actual     = AstPrinter.render(graphQL)
    val expected   = """
                     |type ConnectionSpecOptionalSequences {
                     |  a1: [Int!]
                     |  a2: [Int]
                     |  a3: [Int]!
                     |  a4: [[Int!]]!
                     |}
                     |type ConnectionSpecOptionals {
                     |  a1: Int
                     |  a2: String
                     |  a3: Boolean
                     |  a4: Float
                     |  a5: Int
                     |}
                     |type ConnectionSpecRoot {
                     |  optionalSequences: ConnectionSpecOptionalSequences!
                     |  optionals: ConnectionSpecOptionals!
                     |  scalars: ConnectionSpecScalars!
                     |  sequences: ConnectionSpecSequences!
                     |}
                     |type ConnectionSpecScalars {
                     |  a1: Int!
                     |  a2: String!
                     |  a3: Boolean!
                     |  a4: Float!
                     |}
                     |type ConnectionSpecSequences {
                     |  a1: [Int!]!
                     |  a2: [String!]!
                     |  a3: [Boolean!]!
                     |  a4: [Float!]!
                     |  a5: [ConnectionSpecScalars!]!
                     |  a6: [ConnectionSpecOptionals!]!
                     |  a7: [[Int!]!]!
                     |}
                     |type Query {
                     |  root: ConnectionSpecRoot!
                     |}
                     |""".stripMargin

    assertTrue(actual == expected)
  }))
}
