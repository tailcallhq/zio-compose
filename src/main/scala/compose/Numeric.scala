package compose

object Numeric {

  sealed trait Is[A]
  object Is {
    case object NumericInt    extends Is[Int]
    case object NumericDouble extends Is[Double]
    case object NumericLong   extends Is[Long]
  }

  sealed trait Operation

  object Operation {
    case object Addition       extends Operation
    case object Multiplication extends Operation
    case object Subtraction    extends Operation
    case object Division       extends Operation
  }
}
