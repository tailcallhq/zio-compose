package compose

object Numeric {

  sealed trait Operation
  object Operation {
    case object Add                extends Operation
    case object Multiply           extends Operation
    case object Subtract           extends Operation
    case object Divide             extends Operation
    case object GreaterThan        extends Operation
    case object GreaterThanEqualTo extends Operation
  }
}
