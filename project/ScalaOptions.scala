import sbt._

object ScalaOptions {
  def apply(version: String): Seq[String] = CrossVersion.partialVersion(version) match {
    case Some((2, _)) => Seq("-Ywarn-unused", "-Werror", "-feature", "-language:reflectiveCalls")
    case _            => Seq()
  }
}
