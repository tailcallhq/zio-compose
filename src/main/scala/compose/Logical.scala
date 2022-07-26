package compose

object Logical {
  sealed trait Operation
  case object And extends Operation
  case object Or  extends Operation
}
