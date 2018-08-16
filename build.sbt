lazy val scalaz =
  ProjectRef(uri("git:https://github.com/scalaz/scalaz.git#series/8.0.x"), "baseJVM")

val testzVersion   = "0.0.4"
val monocleVersion = "1.5.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scalaz-schema",
    libraryDependencies ++= Seq(
      "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
      "org.scalaz"                 %% "testz-core"    % testzVersion % "test",
      "org.scalaz"                 %% "testz-stdlib"  % testzVersion % "test",
      "org.scalaz"                 %% "testz-runner"  % testzVersion % "test"
    ).map(_.exclude("org.scalaz", "scalaz"))
  )
  .dependsOn(scalaz)

lazy val scalacheck = project.in(file("modules/scalacheck")).dependsOn(root)
