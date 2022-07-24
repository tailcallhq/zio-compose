package com.tusharmath.compose

import zio.{ZIO, ZIOAppDefault}
import zio.schema.{DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.schema.DeriveSchema.gen

object Example extends ZIOAppDefault {

  import Lambda._

  def createUser: Unit ~> User = always(User("John", 5))

  def demographic: User ~> String = ifElse(between(13, 19) <<< User.age)(
    isTrue = always("Is a teen").accept[User],
    isFalse = always("Is not a teen").accept[User],
  )

  // demographic: User ~> String
  // createUser: Unit ~> User
  def program: Unit ~> String = {
    demographic <<< createUser
  }

  override def run =
    for {

      // Serialize the program to JSON
      json <- ZIO.succeed(program.executable.json)

      _ <- ZIO.succeed(println(json))

      // Deserialize the program from JSON
      exe <- Executable.fromJson(json)

      // Execute the program
      unit = Schema.primitive[Unit].toDynamic(())
      res <- exe.unsafeExecute(unit)

      // Serialize and print the output
      resJson <- ZIO.succeed(
        new String(JsonCodec.encode(Schema[DynamicValue])(res).toArray),
      )
      _       <- ZIO.succeed(println(resJson))
    } yield ()

  case class User(name: String, age: Int)
  object User {
    val (name, age) = Schema[User]
      .makeAccessors(LambdaAccessor)
      .asInstanceOf[(User ~> String, User ~> Int)]
  }

}
