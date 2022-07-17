import Dependencies._

val scala3Version = "2.13.8"

lazy val root = project
  .in(file("."))
  .settings(
    name                := "GraphQLCompose",
    version             := "0.1.0-SNAPSHOT",
    scalaVersion        := scala3Version,
    libraryDependencies := Seq(
      ZIOCore,
      ZIOSchema,
      ZIOSchemaDerivation,
      ZIOTest % Test,
    ),
  )
