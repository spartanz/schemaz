lazy val scalaz =
  ProjectRef(uri("git:https://github.com/scalaz/scalaz.git#series/8.0.x"), "baseJVM")

val testzVersion   = "0.0.4"
val monocleVersion = "1.5.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scalaz-schema"
  )
  .aggregate(
    core,
    scalacheck
  )
  .dependsOn(scalaz)

lazy val core = project
  .in(file("modules/core"))
  .settings(
    name := "scalaz-schema-core",
    libraryDependencies ++= (commonDependencies ++ Seq(
      "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion
    )).map(_.exclude("org.scalaz", "scalaz"))
  )

lazy val scalacheck = project
  .in(file("modules/scalacheck"))
  .settings(
    name := "scalaz-schema-scalacheck",
    libraryDependencies ++= commonDependencies ++ Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.0"
    )
  )
  .dependsOn(core)

lazy val commonDependencies = Seq(
  "org.scalaz" %% "testz-core"   % testzVersion % "test",
  "org.scalaz" %% "testz-stdlib" % testzVersion % "test",
  "org.scalaz" %% "testz-runner" % testzVersion % "test"
)
