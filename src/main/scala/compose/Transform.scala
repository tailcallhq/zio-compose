package compose

import zio.schema.Schema

sealed trait Transform[A, B]
object Transform {
  case class Constructor[A, B, I](f: A ~> I, i: Schema[I], g: (B, I) ~> B) extends Transform[A, B]
  def apply[A, B, I](f: A ~> I, g: (B, I) ~> B)(implicit i: Schema[I]): Transform[A, B] = Constructor(f, i, g)
}
