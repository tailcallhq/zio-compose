package com.tusharmath.compose

import zio.{ZIO, ZIOAppDefault}
import zio.schema.{DynamicValue, Schema}
import zio.schema.codec.JsonCodec

object Example extends ZIOAppDefault {
  import Remote._
  val program = {

    ifElse(lit(1) + lit(2) > lit(1))(
      ifTrue = lit("Greater!"),
      ifFalse = lit("Lesser!"),
    )
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
      res <- exe.execute

      // Serialize and print the output
      resJson <- ZIO.succeed(
        new String(JsonCodec.encode(Schema[DynamicValue])(res).toArray),
      )
      _       <- ZIO.succeed(println(resJson))
    } yield ()

}
