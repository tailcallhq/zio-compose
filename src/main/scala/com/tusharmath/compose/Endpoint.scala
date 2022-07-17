package com.tusharmath.compose

final case class Endpoint(host: String, port: Int, path: String, body: Array[Byte], protocol: Protocol)

sealed trait Protocol
object Protocol {
  case class Http(method: String) extends Protocol
}
