package com.tusharmath.compose

import zio.{ZIO, ZIOAppDefault}
import zio.schema.{DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.schema.DeriveSchema.gen

object Main extends ZIOAppDefault {

  val unit: DynamicValue = Schema.primitive[Unit].toDynamic(())

  import Lambda._
  def userMap = Map(
    1 -> User("John", 30),
    2 -> User("Jane", 25),
    3 -> User("Jack", 20),
    4 -> User("Jill", 15),
  )

  def negate: Int ~> Int = partial2(mul, -1)

  override def run =
    for {
      json    <- ZIO.succeed(program.executable.json)
      exe     <- ExecutionPlan.fromJson(json)
      res     <- exe.unsafeExecute(unit)
      resJson <- ZIO.succeed(
        new String(JsonCodec.encode(Schema[DynamicValue])(res).toArray),
      )
      _       <- ZIO.succeed(println(resJson))
    } yield ()

  def program = ifElse(always(true))(always("Hey!"), always("Bye"))

  case class User(name: String, age: Int)

}
