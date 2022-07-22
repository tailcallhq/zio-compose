package com.tusharmath.compose

import zio.{ZIO, ZIOAppDefault}
import zio.schema.{DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.schema.DeriveSchema.gen

object Example extends ZIOAppDefault {
  val unit: DynamicValue = Schema.primitive[Unit].toDynamic(())

  import Lambda._

  override def run =
    for {

      // Serialize the program to JSON
      json <- ZIO.succeed(program.executable.json)

      // Deserialize the program from JSON
      exe <- ExecutionPlan.fromJson(json)

      // Execute the program
      res <- exe.unsafeExecute(unit)

      // Serialize and print the output
      resJson <- ZIO.succeed(
        new String(JsonCodec.encode(Schema[DynamicValue])(res).toArray),
      )
      _       <- ZIO.succeed(println(resJson))
    } yield ()

  def isLT20: Int ~> Boolean = gt <<< partial[Int, Int](20)
  def isGT12: Int ~> Boolean = lt <<< partial[Int, Int](12)
  def isGT60: Int ~> Boolean = lt <<< partial[Int, Int](60)

  def userAge = User.age

  def isTeen: Unit ~> Boolean =
    and <<<
      (isGT12 zip isLT20) <<<
      unary <<<
      userAge <<<
      always(User("John", 15))

  def demographic = ifElse(isTeen)(
    isTrue = always("Is a teen"),
    isFalse = always("Is not a teen"),
  )

  def program: Unit ~> Int = converge1(add)(identity, identity) <<< always(100)

  case class User(name: String, age: Int)
  object User {
    val (name, age) = Schema[User]
      .makeAccessors(LambdaAccessor)
      .asInstanceOf[(User ~> String, User ~> Int)]
  }

}
