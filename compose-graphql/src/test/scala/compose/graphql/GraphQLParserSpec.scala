package compose.graphql
import compose.graphql.ast.OperationDefinition
import compose.graphql.ast.OperationDefinition._
import zio.Chunk
import zio.test._

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
        val actual = OperationDefinition.syntax.parseString("query { a (user : 10) { b } }")

        val expected = OperationDefinition(
          operation = OperationDefinition.QueryOperation,
          name = None,
          selectionSet = Chunk(Field(
            name = "a",
            arguments = Chunk(Argument(name = "user", value = IntValue(value = 10))),
            selection = Chunk(Field(name = "b", arguments = Chunk(), selection = Chunk())),
          )),
        )

        assertTrue(actual == Right(expected))
      },
    )
  }
}
