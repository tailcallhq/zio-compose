import Dependencies._

Global / scalaVersion := "2.13.8"
val libVersion = "0.1.0-SNAPSHOT"

// Projects
lazy val root = (project in file(".")).aggregate(zioCompose, zioComposeMacros)

lazy val zioCompose = project
  .in(file("./compose"))
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
    publish / skip      := true,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  )
  .dependsOn(zioComposeMacros)

lazy val zioComposeMacros = project
  .in(file("./compose-macros"))
  .settings(
    name                := "zio-compose-macros",
    fork                := true,
    publish / skip      := true,
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
