package compose.graphql

import compose.Interpreter
import compose.graphql.Node.OperationDefinition
import compose.graphql.internal.JsonUtility
import zio._
import zio.json.ast.Json
import zio.schema.DynamicValue

final class Executor(interpreter: Interpreter) {

  def execute(edge: Graph, operation: OperationDefinition): Task[Json] = {
    val edges: Map[String, Graph.Cons] = edge.cons.map(e => e.name -> e).toMap
    operation.operation match {
      case Node.QueryOperation =>
        val map = ZIO.foreach(operation.selectionSet) { field =>
          edges.get(field.name) match {
            case Some(edge) => interpreter.evalDynamic(edge.executable, DynamicValue(()))
                .map(d => (field.name, JsonUtility.fromDynamicValue(d)))
            case None       => ZIO.fail(new RuntimeException(s"Missing field: ${field.name}"))
          }
        }

        map.map(chunk => Json.Obj(chunk))

      case Node.MutationOperation     => ???
      case Node.SubscriptionOperation => ???
    }
  }
}

object Executor {
  def execute(edge: Graph, operation: OperationDefinition): ZIO[Any, Throwable, Json] = for {
    i <- Interpreter.inMemory
    e = new Executor(i)
    d <- e.execute(edge, operation)
  } yield d

}
