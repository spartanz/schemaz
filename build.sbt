val testzVersion   = "0.0.4"
val monocleVersion = "1.5.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scalaz-schema"
  )
  .aggregate(
    core,
    scalacheck,
    tests
  )

lazy val core = project
  .in(file("modules/core"))
  .settings(
    name := "scalaz-schema-core",
    libraryDependencies ++= Seq(
      "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion
    ).map(_.exclude("org.scalaz", "scalaz"))
  )

lazy val scalacheck = project
  .in(file("modules/scalacheck"))
  .settings(
    name := "scalaz-schema-scalacheck",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.0"
    )
  )
  .dependsOn(core)

lazy val tests = project
  .in(file("modules/tests"))
  .settings(
    name := "scalaz-schema-tests",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "testz-core"   % testzVersion,
      "org.scalaz" %% "testz-stdlib" % testzVersion,
      "org.scalaz" %% "testz-runner" % testzVersion
    ).map(_.exclude("org.scalaz", "scalaz"))
  )
  .dependsOn(core, scalacheck)
