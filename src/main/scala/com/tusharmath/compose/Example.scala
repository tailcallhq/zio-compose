package com.tusharmath.compose

import zio.{ZIO, ZIOAppDefault}
import zio.schema.{DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.schema.DeriveSchema.gen

object Example extends ZIOAppDefault {
  val unit: DynamicValue = Schema.primitive[Unit].toDynamic(())

  import Lambda._

  def program = {
    always(User("Tushar", 30)) >>> User.age >>> partial[Int, Int](20) >>> gte
  }

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

  case class User(name: String, age: Int)
  object User {
    val (name, age) = Schema[User]
      .makeAccessors(LambdaAccessor)
      .asInstanceOf[(User ~> String, User ~> Int)]
  }

}
