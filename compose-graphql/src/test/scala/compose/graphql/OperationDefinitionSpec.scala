package compose.graphql
import compose.graphql.ast.OperationDefinition
import compose.graphql.ast.OperationDefinition._
import zio.Chunk
import zio.test.Assertion.{equalTo, isRight}
import zio.test._

object OperationDefinitionSpec extends ZIOSpecDefault {
  def spec = {
    suite("Syntax")(
      test("anyName") {
        val input = Seq("a", "_1", "hello", "world", "helloWorld", "hello_world", "helloWorld123")

        checkAll(Gen.fromIterable(input)) { input =>
          val actual = OperationDefinition.anyName.parseString(input)
          assert(actual)(isRight(equalTo(input)))
        }
      },
      test("query") {
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
      test("query with args") {
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
      test("query with multiple args") {
        val actual = OperationDefinition.syntax
          .parseString("query { a (age : 10,isValid: true, name: \"Jose\") { b } }")

        val expected = OperationDefinition(
          operation = OperationDefinition.QueryOperation,
          name = None,
          selectionSet = Chunk(Field(
            name = "a",
            arguments = Chunk(
              Argument(name = "age", value = IntValue(value = 10)),
              Argument(name = "isValid", value = BooleanValue(value = true)),
              Argument(name = "name", value = StringValue(value = "Jose")),
            ),
            selection = Chunk(Field(name = "b", arguments = Chunk(), selection = Chunk())),
          )),
        )

        assertTrue(actual == Right(expected))
      },
    )
  }
}
