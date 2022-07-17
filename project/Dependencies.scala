object Dependencies {
  import sbt._
  val zioVersion       = "2.0.0"
  val zioSchemaVersion = "0.2.0"

  val ZIOCore             = "dev.zio" %% "zio"                   % zioVersion
  val ZIOTest             = "dev.zio" %% "zio-test"              % zioVersion
  val ZIOSchema           = "dev.zio" %% "zio-schema"            % zioSchemaVersion
  val ZIOSchemaDerivation = "dev.zio" %% "zio-schema-derivation" % zioSchemaVersion
}
