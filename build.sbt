import Dependencies._

ThisBuild / scalaVersion := "2.13.8"

// Projects
lazy val root = project
  .in(file("."))
  .settings(
    name                := "GraphQLCompose",
    version             := "0.1.0-SNAPSHOT",
    fork                := true,
    libraryDependencies := Seq(
      ZIOCore,
      ZIOSchema,
      ZIOSchemaJson,
      ZIOSchemaDerivation,
      ZIOTest % Test,
    ),
    publish / skip := true
  )

// Flags
Global / semanticdbEnabled    := true
Global / onChangedBuildSource := ReloadOnSourceChanges
Global / scalacOptions        := Seq(
  "-Ywarn-unused:imports",
  "-Werror",
)
