package compose.graphql
import compose.graphql.ast.OperationDefinition
import compose.graphql.ast.OperationDefinition._
import zio.test._
import zio.Chunk
import zio.test.TestAspect.failing

object GraphQLParserSpec extends ZIOSpecDefault {
  def spec = {
    suite("GraphQLParser")(
      test("parse") {
        val actual = OperationDefinition.syntax
          .parseString("query { a { b c d } b {c d} c { e { f } } }")

        val expected = OperationDefinition(
          operation = QueryOperation,
          None,
          selectionSet = Chunk(
            Field("a", selection = Chunk(Field("b"), Field("c"), Field("d"))),
            Field("b", selection = Chunk(Field("c"), Field("d"))),
            Field("c", selection = Chunk(Field("e", selection = Chunk(Field("f", Chunk()))))),
          ),
        )

        assertTrue(actual == Right(expected))
      },
      test("parse with args") {
        val actual = OperationDefinition.syntax.parseString("query { a (user: 10) { b } }")

        val expected = OperationDefinition(
          operation = QueryOperation,
          None,
          selectionSet = Chunk(Field("a", selection = Chunk(Field("b")))),
        )

        assertTrue(actual == Right(expected))
      } @@ failing,
    )
  }
}
