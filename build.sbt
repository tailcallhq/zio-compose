import Dependencies._

Global / scalaVersion := "2.13.8"

val libVersion = "0.1.0-SNAPSHOT"

def publishSettings(projectName: String) = Seq(
  publish / skip := false,
  version        := libVersion,
  name           := projectName,
  versionScheme  := Some("early-semver"),
  publishTo      := Some("Github Package Registry" at s"https://maven.pkg.github.com/tusharmath/zio-compose"),
)

// Projects
lazy val root = (project in file("."))
  .aggregate(zioCompose, zioComposeMacros)
  .settings(
    publish / skip := true,
  )

lazy val zioCompose = project
  .in(file("./compose"))
  .settings(publishSettings("zio-compose"))
  .settings(
    fork                := true,
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
  .settings(publishSettings("zio-compose-macros"))
  .settings(
    fork                := true,
    libraryDependencies := Seq(
      ZIOSchema,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
    ),
  )

// Flags
Global / semanticdbEnabled    := true
Global / onChangedBuildSource := ReloadOnSourceChanges
Global / scalacOptions        := Seq(
  "-Ywarn-unused:imports",
  "-Werror",
  "-feature",
  "-language:reflectiveCalls",
)
