import Dependencies._

// Flags
Global / semanticdbEnabled          := true
Global / onChangedBuildSource       := ReloadOnSourceChanges
Global / scalacOptions              := Seq(
  "-Ywarn-unused:imports",
  "-Werror",
  "-feature",
  "-language:reflectiveCalls",
)
Global / scalaVersion               := "2.13.8"
ThisBuild / versionScheme           := Some("early-semver")
ThisBuild / dynverSonatypeSnapshots := true

lazy val publishSettings = Seq(
  githubOwner       := "tusharmath",
  githubRepository  := "zio-compose",
  githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("GITHUB_TOKEN"),
  organization      := "com.tusharmath",
)

// Projects
lazy val root = (project in file("."))
  .aggregate(zioCompose, zioComposeMacros, zioComposeExamples)
  .settings(name := "root", publish / skip := true)
  .settings(publishSettings)

lazy val zioCompose = project
  .in(file("./compose"))
  .settings(publishSettings)
  .settings(
    name                := "zio-compose",
    libraryDependencies := Seq(
      ZIOCore,
      ZIOSchema,
      ZIOSchemaJson,
      ZIOSchemaDerivation,
      ZIOTest,
      ZIOTestSbt,
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  )
  .dependsOn(zioComposeMacros)

lazy val zioComposeMacros = project
  .in(file("./compose-macros"))
  .settings(publishSettings)
  .settings(
    name                := "zio-compose-macros",
    libraryDependencies := Seq(
      ZIOSchema,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
    ),
  )

lazy val zioComposeExamples = project
  .in(file("./compose-examples"))
  .dependsOn(zioCompose, zioComposeMacros)
  .settings(publishSettings)
  .settings(
    name           := "zio-compose-examples",
    publish / skip := true,
    fork           := true,
  )
