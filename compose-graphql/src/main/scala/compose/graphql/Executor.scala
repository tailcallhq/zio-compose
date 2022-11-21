package compose.graphql

import compose.Interpreter
import compose.graphql.Node.OperationDefinition
import compose.graphql.internal.JsonUtility
import zio._
import zio.json.ast.{Json, JsonCursor}
import zio.schema.DynamicValue

final class Executor(interpreter: Interpreter) {

  private def resolveSelection(fields: Chunk[Node.Field], json: Json): Task[Json] = {
    if (fields.isEmpty) ZIO.succeed(json)
    else json.get(JsonCursor.isArray) match {
      case Right(arr) => ZIO.foreach(arr.elements) { json => resolveSelection(fields, json) }
          .map(Json.Arr(_))
      case Left(_)    => ZIO.foreach(fields) { field =>
          val cursor = JsonCursor.field(field.name)
          json.get(cursor) match {
            case Right(value)  => resolveSelection(field.selection, value)
                .map(json => field.name -> json)
            case Left(message) => ZIO.fail(new RuntimeException(message))
          }
        }.map(chunk => Json.Obj(chunk))
    }
  }

  def execute(edge: Graph, operation: OperationDefinition): Task[Json] = {
    val edges: Map[String, Graph.Cons] = edge.cons.map(e => e.name -> e).toMap
    operation.operation match {
      case Node.QueryOperation =>
        val map = ZIO.foreach(operation.selectionSet) { field =>
          edges.get(field.name) match {
            case Some(edge) => interpreter.evalDynamic(edge.executable, DynamicValue(()))
                .flatMap { d =>
                  resolveSelection(field.selection, JsonUtility.fromDynamicValue(d))
                    .map(json => (field.name, json))
                }
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
