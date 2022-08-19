package compose.operation

sealed trait OpticalOp
object OpticalOp {

  final case class GetPath(path: List[String]) extends OpticalOp

  final case class SetPath(path: List[String]) extends OpticalOp
}
