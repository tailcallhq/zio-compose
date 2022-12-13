package compose.graphql

import caliban.schema.Step

import compose.Interpreter
import zio._
import caliban.introspection.adt._

final class SchemaGenerator(graph: Graph, i: Interpreter) {
  self =>

  private def generateSchema(graph: Graph): caliban.schema.Schema[Any, Graph] =
    new caliban.schema.Schema[Any, Graph] {
      override protected[this] def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        new TypeGenerator(graph).__type

      override def resolve(value: Graph): Step[Any] = new StepGenerator(graph, i).resolve
    }

  def generateSchema: caliban.schema.Schema[Any, Graph] = generateSchema(graph)
}

object SchemaGenerator {
  def make(graph: Graph): UIO[SchemaGenerator] = for {
    i <- Interpreter.inMemory
  } yield new SchemaGenerator(graph, i)

  def gen(graph: Graph): UIO[caliban.schema.Schema[Any, Graph]] = make(graph).map(_.generateSchema)
}
