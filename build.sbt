import Dependencies._

Global / scalaVersion := "2.13.8"

val libVersion = "0.1.0-SNAPSHOT"

lazy val publishSettings = Seq(
  publish / skip    := false,
  publishTo         := Some("Github Package Registry" at "https://maven.pkg.github.com/tusharmath/zio-compose"),
  versionScheme     := Some("early-semver"),
  githubWorkflowEnv := Map("GITHUB_TOKEN" -> "${{ secrets.GITHUB_TOKEN }}"),
)

// Projects
lazy val root = (project in file("."))
  .aggregate(zioCompose, zioComposeMacros)
  .settings(
    publish / skip := true,
  )

lazy val zioCompose = project
  .in(file("./compose"))
  .settings(publishSettings)
  .settings(
    name                := "zio-compose",
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
  .settings(publishSettings)
  .settings(
    name                := "zio-compose-macros",
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
