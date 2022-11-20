package compose.graphql

import compose.{Lambda, ~>}
import zio.schema.{DeriveSchema, Schema}
import zio.test.{ZIOSpecDefault, assertTrue}

object GraphSpec extends ZIOSpecDefault {

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

  def spec = suite("EdgeSpec")(suite("schema")(test("render") {
    val connection = Graph[Unit, Unit]("root", die[Unit, Root] <<< Lambda._1)
    val graphQL    = NodeFactory.gen(connection)
    val actual     = NodePrinter.render(graphQL)
    val expected   = """
                     |type OptionalSequences {
                     |  a1: [Int!]
                     |  a2: [Int]
                     |  a3: [Int]!
                     |  a4: [[Int!]]!
                     |}
                     |type Optionals {
                     |  a1: Int
                     |  a2: String
                     |  a3: Boolean
                     |  a4: Double
                     |  a5: Int
                     |}
                     |type Query {
                     |  root: Root!
                     |}
                     |type Root {
                     |  optionalSequences: OptionalSequences!
                     |  optionals: Optionals!
                     |  scalars: Scalars!
                     |  sequences: Sequences!
                     |}
                     |type Scalars {
                     |  a1: Int!
                     |  a2: String!
                     |  a3: Boolean!
                     |  a4: Double!
                     |}
                     |type Sequences {
                     |  a1: [Int!]!
                     |  a2: [String!]!
                     |  a3: [Boolean!]!
                     |  a4: [Double!]!
                     |  a5: [Scalars!]!
                     |  a6: [Optionals!]!
                     |  a7: [[Int!]!]!
                     |}
                     |""".stripMargin

    assertTrue(actual == expected)
  }))
}
