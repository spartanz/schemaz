lazy val scalaz = ProjectRef(uri("git:https://github.com/scalaz/scalaz.git#series/8.0.x"), "baseJVM")

lazy val root = project.in(file("."))
  .settings(name := "scalaz-schema")
  .dependsOn(scalaz)

lazy val scalacheck = project.in(file("modules/scalacheck")).dependsOn(root)
