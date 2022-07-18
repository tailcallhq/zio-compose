package com.tusharmath.compose

import com.tusharmath.compose.GraphQLSpec.test
import zio.test.{assertTrue, ZIOSpecDefault}

object GraphQLSpec extends ZIOSpecDefault {
  override def spec = test("Some test") {
    assertTrue(true)
  }
}
