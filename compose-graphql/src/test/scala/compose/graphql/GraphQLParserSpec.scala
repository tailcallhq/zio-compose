package compose.graphql
import zio.test._
import compose.graphql.GraphQLParser
import zio.Chunk

object GraphQLParserSpec extends ZIOSpecDefault {
  import Ast._
  def spec = {
    suite("GraphQLParser")(test("parse") {
      val actual = GraphQLParser.querySyntax
        .parseString("query { a { b c d } b {c d} c { e { f } } }")

      val expected = Definition.OperationDefinition(
        operation = Operation.Query,
        None,
        selectionSet = Chunk(
          Field("a", Chunk(Field("b"), Field("c"), Field("d"))),
          Field("b", Chunk(Field("c"), Field("d"))),
          Field("c", Chunk(Field("e", Chunk(Field("f", Chunk()))))),
        ),
      )

      assertTrue(actual == Right(expected))
    })
  }
}
