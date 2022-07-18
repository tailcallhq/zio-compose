package com.tusharmath.compose

import zio.schema.{DeriveSchema, DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.{ZIO, ZIOAppDefault}

object Main extends ZIOAppDefault {

  case class Round(name: String, id: Round.Id)
  object Round {
    case class Id(id: Long)
    implicit val schema  = DeriveSchema.gen[Round]
    implicit val matchId = DeriveSchema.gen[Id]
    val accessors        = schema.makeAccessors(GraphQL.Accessors)
  }

  case class Contest(
    name: String,
    id: Contest.Id,
    entryFee: Double,
    size: Long,
//    users: List[User.Id],
    roundId: Round.Id,
  )

  object Contest {

    case class Id(id: Long)

    implicit val schema    = DeriveSchema.gen[Contest]
    implicit val contestId = DeriveSchema.gen[Id]

    val (name, id, entryFee, size, roundId) = schema.makeAccessors(GraphQL.Accessors)
  }

  case class User(name: String, id: User.Id, age: Int)
  object User {
    case class Id(id: Long)

    implicit val schema = DeriveSchema.gen[User]
    implicit val userId = DeriveSchema.gen[Id]

    val (name, id, age) = schema.makeAccessors(GraphQL.Accessors)
  }

  val getUser1 = GraphQL.constant(User("Tushar Mathur", User.Id(1), 30))

  val getUser2 = GraphQL.constant(User("Aiswarya Prakasan", User.Id(2), 90))

  val getRound = GraphQL.fromMap {
    Map(
      Round.Id(1) -> Round("Round 1", Round.Id(1)),
      Round.Id(2) -> Round("Round 2", Round.Id(2)),
      Round.Id(3) -> Round("Round 3", Round.Id(3)),
    )
  }

  val getContest = GraphQL.constant(Contest("Contest 1", Contest.Id(1), 100.0, 10, Round.Id(1)))

  // From contest prepare round details
  val program = getContest >>> Contest.roundId >>> getRound

  // Execution plan that can be transferred over the wire
  val plan = ExecutionPlan.fromGraphQL(program)

  val encoded = new String(JsonCodec.encode(ExecutionPlan.schema)(plan).toArray)

  val dUnit = implicitly[Schema[Unit]].toDynamic(())

  override def run = for {
    result <- Executor.execute(plan, dUnit)
    _      <- ZIO.attempt(println(new String(JsonCodec.encode(implicitly[Schema[DynamicValue]])(result).toArray)))
  } yield ()
}
