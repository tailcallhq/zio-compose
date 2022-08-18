ThisBuild / organization         := "com.tusharmath"
ThisBuild / organizationName     := "tusharmath"
ThisBuild / organizationHomepage := Some(url("https://tusharmath.com"))

ThisBuild / scmInfo     := Some(
  ScmInfo(
    url("https://github.com/tusharmath/zio-compose"),
    "scm:git@github.tusharmath/zio-compose.git",
  ),
)

ThisBuild / developers  := List(
  Developer(
    id = "tusharmath",
    name = "Tushar Mathur",
    email = "tusharmath@gmail.com",
    url = url("https://tusharmath.com"),
  ),
)

ThisBuild / description := "A Scala DSL for distributed computing"
ThisBuild / licenses    := List(("MIT License", new URL("https://github.com/dream11/zio-http/blob/master/LICENSE")))
ThisBuild / homepage    := Some(url("https://github.com/tusharmath/zio-compose"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo         := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / publishMavenStyle := true

ThisBuild / versionScheme := Some("early-semver")

ThisBuild / credentials += Credentials(
  "GnuPG Key ID",
  "gpg",
  "22A809FF5B03D5B9506FF7E4376FF267DC5370B0", // key identifier
  "ignored",                                  // this field is ignored; passwords are supplied by pinentry
)
