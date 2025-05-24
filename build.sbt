import Dependencies._

// Flags
Global / semanticdbEnabled        := true
Global / onChangedBuildSource     := ReloadOnSourceChanges
Global / scalacOptions            := Seq(
  "-Ywarn-unused",
  "-Werror",
  "-feature",
  "-language:reflectiveCalls",
  "-deprecation",
)
Global / scalaVersion             := "2.13.8"
ThisBuild / versionScheme         := Some("early-semver")
ThisBuild / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches += RefPredicate.StartsWith(Ref.Tag("v"))
ThisBuild / githubWorkflowPublish := Seq(WorkflowStep.Sbt(List("ci-release")))
// Override default job setup to use secure action versions - fixes CVE-2024-42471
ThisBuild / githubWorkflowJobSetup := Seq(
  WorkflowStep.Use(
    UseRef.Public("actions", "checkout", "v4.2.2"),
    params = Map("fetch-depth" -> "0")
  ),
  WorkflowStep.Use(
    UseRef.Public("actions", "setup-java", "v4.7.1"),
    params = Map(
      "distribution" -> "temurin",
      "java-version" -> "${{ matrix.java }}"
    )
  ),
  WorkflowStep.Use(
    UseRef.Public("actions", "cache", "v4.2.3"),
    params = Map(
      "path" -> """|
        ~/.sbt
        ~/.ivy2/cache
        ~/.coursier/cache/v1
        ~/.cache/coursier/v1
        ~/AppData/Local/Coursier/Cache/v1
        ~/Library/Caches/Coursier/v1""".stripMargin,
      "key" -> "${{ runner.os }}-sbt-cache-v2-${{ hashFiles('**/*.sbt') }}-${{ hashFiles('project/build.properties') }}"
    )
  )
)

// Override upload/download steps to use secure artifact actions
ThisBuild / githubWorkflowGeneratedUploadSteps := Seq(
  WorkflowStep.Run(
    commands = List("tar cf targets.tar target compose-examples/target compose/target compose-macros/target compose-graphql/target project/target"),
    name = Some("Compress target directories")
  ),
  WorkflowStep.Use(
    UseRef.Public("actions", "upload-artifact", "v4.6.2"),
    params = Map(
      "name" -> "target-${{ matrix.os }}-${{ matrix.scala }}-${{ matrix.java }}",
      "path" -> "targets.tar"
    )
  )
)

ThisBuild / githubWorkflowGeneratedDownloadSteps := Seq(
  WorkflowStep.Use(
    UseRef.Public("actions", "download-artifact", "v4.3.0"),
    params = Map(
      "name" -> "target-${{ matrix.os }}-2.13.8-${{ matrix.java }}"
    )
  ),
  WorkflowStep.Run(
    commands = List(
      "tar xf targets.tar",
      "rm targets.tar"
    ),
    name = Some("Inflate target directories (2.13.8)")
  )
)

ThisBuild / githubWorkflowPublish := Seq(WorkflowStep.Sbt(
  List("ci-release"),
  env = Map(
    "PGP_PASSPHRASE"    -> "${{ secrets.PGP_PASSPHRASE }}",
    "PGP_SECRET"        -> "${{ secrets.PGP_SECRET }}",
    "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
    "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
  ),
))

inThisBuild(List(
  organization := "com.tusharmath",
  homepage     := Some(url("https://github.com/tusharmath/zio-compose")),
  licenses     := List("MIT" -> url("https://github.com/tusharmath/zio-compose/blob/main/LICENSE")),
  developers   := List(
    Developer("tusharmath", "Tushar Mathur", "tusharmath@gmail.com", url("https://tusharmath.com")),
  ),
))

// Projects
lazy val root = (project in file("."))
  .aggregate(zioCompose, zioComposeMacros, zioComposeExamples, zioComposeGraphQL)
  .settings(name := "root", publish / skip := true)

lazy val zioCompose = project.in(file("./compose")).settings(
  name                := "zio-compose",
  libraryDependencies := Netty ++ Seq(
    ZIOCore,
    ZIOSchema,
    ZIOSchemaJson,
    ZIOSchemaDerivation,
    ZIOTest,
    ZIOTestSbt,
  ),
).dependsOn(zioComposeMacros)

lazy val zioComposeMacros = project.in(file("./compose-macros")).settings(
  name                := "zio-compose-macros",
  libraryDependencies := Seq(
    ZIOSchema,
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
  ),
)

lazy val zioComposeExamples = project.in(file("./compose-examples"))
  .dependsOn(zioCompose, zioComposeMacros)
  .settings(name := "zio-compose-examples", publish / skip := true)

lazy val zioComposeGraphQL = project.in(file("./compose-graphql"))
  .dependsOn(zioCompose, zioComposeMacros).settings(
    name                := "zio-compose-graphql",
    publish / skip      := true,
    libraryDependencies := Seq(
      ZIOCore,
      ZIOSchema,
      ZIOSchemaJson,
      ZIOSchemaDerivation,
      ZIOTest,
      ZIOTestSbt,
      PPrint,
      Caliban
    ),
  )
