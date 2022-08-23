object Dependencies {
  import sbt._
  val zioVersion       = "2.0.1"
  val zioSchemaVersion = "0.2.1"

  val ZIOCore             = "dev.zio" %% "zio"                   % zioVersion
  val ZIOTest             = "dev.zio" %% "zio-test"              % zioVersion % Test
  val ZIOTestSbt          = "dev.zio" %% "zio-test-sbt"          % zioVersion % Test
  val ZIOSchema           = "dev.zio" %% "zio-schema"            % zioSchemaVersion
  val ZIOSchemaJson       = "dev.zio" %% "zio-schema-json"       % zioSchemaVersion
  val ZIOSchemaDerivation = "dev.zio" %% "zio-schema-derivation" % zioSchemaVersion
}
