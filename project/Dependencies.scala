object Dependencies {
  import sbt._
  val zioVersion       = "2.0.3"
  val zioSchemaVersion = "0.2.1"
  val nettyVersions    = "4.1.85.Final"
  val calibanVersion   = "2.0.1"
  val pprintVersion    = "0.8.0"

  val ZIOCore             = "dev.zio"               %% "zio"                   % zioVersion
  val ZIOTest             = "dev.zio"               %% "zio-test"              % zioVersion % Test
  val ZIOTestSbt          = "dev.zio"               %% "zio-test-sbt"          % zioVersion % Test
  val ZIOSchema           = "dev.zio"               %% "zio-schema"            % zioSchemaVersion
  val ZIOSchemaJson       = "dev.zio"               %% "zio-schema-json"       % zioSchemaVersion
  val ZIOSchemaDerivation = "dev.zio"               %% "zio-schema-derivation" % zioSchemaVersion
  val PPrint              = "com.lihaoyi"           %% "pprint"                % pprintVersion
  val Caliban             = "com.github.ghostdogpr" %% "caliban"               % calibanVersion
  val ZIOParser           = "dev.zio"               %% "zio-parser"            % "0.1.7"
  val Netty               = Seq(
    "io.netty" % "netty-codec-http"             % nettyVersions,
    "io.netty" % "netty-handler-proxy"          % nettyVersions,
    "io.netty" % "netty-transport-native-epoll" % nettyVersions,
    "io.netty" % "netty-transport-native-epoll" % nettyVersions % Runtime classifier "linux-x86_64",
    "io.netty" % "netty-transport-native-epoll" % nettyVersions % Runtime classifier "linux-aarch_64",
    "io.netty" % "netty-transport-native-kqueue" % nettyVersions,
    "io.netty" % "netty-transport-native-kqueue" % nettyVersions % Runtime classifier "osx-x86_64",
    "io.netty" % "netty-transport-native-kqueue" % nettyVersions % Runtime classifier "osx-aarch_64",
  )
}
