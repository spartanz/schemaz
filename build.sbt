val testzVersion   = "0.0.4"
val monocleVersion = "1.5.0"
val derivingVersion = "1.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scalaz-schema",
    addCompilerPlugin("io.tryp" % "splain" % "0.3.5" cross CrossVersion.patch)
  )
  .aggregate(
    core,
    scalacheck,
    `test-commons`
  )




lazy val core = project
  .in(file("modules/core"))
  .settings(
    name := "scalaz-schema-core",
    libraryDependencies ++= Seq(
      "com.github.julien-truffaut" %% "monocle-core"    % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-macro"   % monocleVersion,
      "org.scalaz"                 %% "scalaz-deriving" % derivingVersion
    ).map(_.exclude("org.scalaz", "scalaz")),
    addCompilerPlugin("io.tryp" % "splain" % "0.3.5" cross CrossVersion.patch)
  )
  .dependsOn(`test-commons` % "test->test")

lazy val scalacheck = project
  .in(file("modules/scalacheck"))
  .settings(
    name := "scalaz-schema-scalacheck",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.0",
      "com.chuusai" %% "shapeless" % "2.3.2"
    ),
    addCompilerPlugin("io.tryp" % "splain" % "0.3.5" cross CrossVersion.patch)
  )
  .dependsOn(core, `test-commons` % "test->test")

lazy val `test-commons` = project
  .in(file("modules/test-commons"))
  .settings(
    name := "scalaz-test-commons",
    libraryDependencies ++= Seq(
      "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion % "test",
      "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion % "test",
      "org.scalaz"                 %% "testz-core"    % testzVersion   % "test",
      "org.scalaz"                 %% "testz-stdlib"  % testzVersion   % "test",
      "org.scalaz"                 %% "testz-runner"  % testzVersion   % "test"
    ).map(_.exclude("org.scalaz", "scalaz")),
    addCompilerPlugin("io.tryp" % "splain" % "0.3.5" cross CrossVersion.patch)
  )
