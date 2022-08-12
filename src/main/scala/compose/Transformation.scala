package compose

sealed trait Transformation[-A, +B] { self => }

object Transformation {
  def apply[A, B, I](f: A ~> I, g: (B, I) ~> B): Transformation[A, B] =
    Constructor(f, g)

  case class Constructor[A, B, I](f: A ~> I, g: (B, I) ~> B) extends Transformation[A, B] {
    def apply(f1: A ~> B): A ~> B = f1.zip(f) >>> (g: (B, I) ~> B)
  }
}
