package com.tusharmath.compose

import zio.ZIO
import zio.ZIOAppDefault
import zio.schema.DynamicValue
import zio.schema.Schema
import zio.schema.codec.JsonCodec

object Main extends ZIOAppDefault {

  val unit: DynamicValue = Schema[Unit].toDynamic(())

  val program: Unit ~> Int = ZLambda.partial2(
    (ZLambda(10) && ZLambda(200)) >>> ZLambda.useWith(ZLambda.mul)(
      ZLambda.partial2(ZLambda.add, 1),
      ZLambda.partial2(ZLambda.add, -1),
    ),
    (),
  )

  override def run =
    for {
      result <- program.executable.unsafeExecute(unit)
      _      <- ZIO.attempt(
        println(
          new String(JsonCodec.encode(Schema[DynamicValue])(result).toArray),
        ),
      )
    } yield ()
}
