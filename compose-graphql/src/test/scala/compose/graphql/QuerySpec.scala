package compose.graphql
import zio.test._
import compose.graphql.ast.Query
import zio.Chunk

object QuerySpec extends ZIOSpecDefault {
  import Query._
  def spec = {
    suite("QuerySpec")(test("parse") {
      val actual = Query.querySyntax.parseString("query { a { b c d } b {c d} c { e { f } } }")

      val expected = Query.Definition.OperationDefinition(
        operation = Operation.Query,
        None,
        selectionSet = Chunk(
          Field(
            "a",
            Chunk(Field("b", Chunk.empty), Field("c", Chunk.empty), Field("d", Chunk.empty)),
          ),
          Field("b", Chunk(Field("c", Chunk.empty), Field("d", Chunk.empty))),
          Field("c", Chunk(Field("e", Chunk(Field(name = "f", Chunk()))))),
        ),
      )

      assertTrue(actual == Right(expected))
    })
  }
}
