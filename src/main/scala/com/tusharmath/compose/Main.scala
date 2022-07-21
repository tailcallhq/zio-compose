package com.tusharmath.compose

import zio.ZIO
import zio.ZIOAppDefault
import zio.schema.DynamicValue
import zio.schema.Schema
import zio.schema.codec.JsonCodec

object Main extends ZIOAppDefault {

  val program = ZLambda.constant(10).zip(ZLambda.constant(5)) >>> ZLambda.add

  override def run =
    for {
      _      <- ZIO.succeed(println(s"Executable: ${program.executable.binary}"))
      result <- program.executable.unsafeExecute(())
      _      <- ZIO.attempt(println(new String(JsonCodec.encode(Schema[DynamicValue])(result).toArray)))
    } yield ()
}
