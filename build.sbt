import Dependencies._

// Projects
lazy val root = project
  .in(file("."))
  .settings(
    name                := "GraphQLCompose",
    version             := "0.1.0-SNAPSHOT",
    scalaVersion        := scala3Version,
    fork                := true,
    libraryDependencies := Seq(
      ZIOCore,
      ZIOSchema,
      ZIOSchemaJson,
      ZIOSchemaDerivation,
      ZIOTest % Test,
    ),
  )

// Flags
Global / semanticdbEnabled    := true
Global / onChangedBuildSource := ReloadOnSourceChanges
Global / scalacOptions        := Seq(
  "-Ywarn-unused:imports",
  "-Werror",
)
val scala3Version = "2.13.8"
