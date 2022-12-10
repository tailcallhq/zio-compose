package compose.graphql

import compose.graphql.ast.OperationDefinition
import compose.{ExecutionPlan, ~>}
import zio.json.ast.Json
import zio.schema.codec.JsonCodec.JsonEncoder
import zio.schema.{DeriveSchema, Schema}
import zio.{Chunk, ZIO}

/**
 * A `GraphQL` represents a connection between two nodes in
 * a graph.
 */

sealed trait Graph {
  self =>
  final def ++(other: Graph): Graph      = Graph.Concat(self, other)
  final def combine(other: Graph): Graph = self ++ other
  final def cons: Chunk[Graph.Cons]      = self match {
    case self: Graph.Cons          => Chunk.single(self)
    case Graph.Concat(left, right) => left.cons ++ right.cons
    case Graph.Empty               => Chunk.empty
  }
  final def binary: Chunk[Byte]          = JsonEncoder.encode(Graph.schema, self)
  final def toJson: String               = new String(binary.toArray)

  final def execute(operation: OperationDefinition): ZIO[Any, Throwable, Json] = Executor
    .execute(self, operation)

  final def execute(query: String): ZIO[Any, Throwable, Json] = for {
    op     <- OperationDefinition.syntax.parseString(query) match {
      case Left(_)      => ZIO.fail(new RuntimeException("Query parse error"))
      case Right(value) => ZIO.succeed(value)
    }
    result <- execute(op)
  } yield result
}

object Graph {
  final case class Cons(
    name: String,
    argType: Schema[Any],
    fromType: Schema[Any],
    toType: Schema[Any],
    executable: ExecutionPlan,
  ) extends Graph

  case object Empty                                  extends Graph
  final case class Concat(left: Graph, right: Graph) extends Graph

  def apply[Arg, From]: PartiallyAppliedConnection[Arg, From] =
    new PartiallyAppliedConnection[Arg, From](())

  final class PartiallyAppliedConnection[Arg, From](val unit: Unit) extends AnyVal {
    def apply[To](name: String, resolve: (Arg, From) ~> To)(implicit
      arg: Schema[Arg],
      from: Schema[From],
      to: Schema[To],
    ): Graph = Cons(
      name,
      arg.asInstanceOf[Schema[Any]],
      from.asInstanceOf[Schema[Any]],
      to.asInstanceOf[Schema[Any]],
      resolve.compile,
    )
  }

  def empty: Graph = Empty

  implicit val schema = DeriveSchema.gen[Graph]
}
