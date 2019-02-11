val testzVersion      = "0.0.4"
val monocleVersion    = "1.5.0"
val derivingVersion   = "1.0.0"
val scalacheckVersion = "1.14.0"

inThisBuild(scalaVersion := "2.12.8")

lazy val root = project
  .in(file("."))
  .settings(
    name := "scalaz-schema"
  )
  .aggregate(
    core,
    generic,
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

lazy val generic = project
  .in(file("modules/generic"))
  .settings(
    name := "scalaz-schema-generic",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-deriving" % derivingVersion
    ).map(_.exclude("org.scalaz", "scalaz"))
  )
  .dependsOn(core)

lazy val scalacheck = project
  .in(file("modules/scalacheck"))
  .settings(
    name := "scalaz-schema-scalacheck",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % scalacheckVersion
    )
  )
  .dependsOn(core)

lazy val playJson = project
  .in(file("modules/play-json"))
  .settings(
    name := "scalaz-schema-play-json",
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.6.10"
    )
  )
  .dependsOn(core)

lazy val tests = project
  .in(file("modules/tests"))
  .settings(
    name := "tests",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "testz-core"   % testzVersion,
      "org.scalaz" %% "testz-stdlib" % testzVersion,
      "org.scalaz" %% "testz-runner" % testzVersion
    )
  )
  .dependsOn(core, scalacheck, generic, playJson)

lazy val microsite = project
  .in(file("microsite"))
  .dependsOn(core, scalacheck)
  .enablePlugins(MicrositesPlugin)
  .settings(
    scalacOptions -= "-Yno-imports",
    scalacOptions ~= { _.filterNot(_.startsWith("-Ywarn")) },
    scalacOptions ~= { _.filterNot(_.startsWith("-Xlint")) },
    skip in publish := true,
    libraryDependencies ++= Seq(
      "com.github.ghik" %% "silencer-lib" % "1.0",
      "commons-io"      % "commons-io"    % "2.6"
    ),
    micrositeFooterText := Some(
      """
        |<p>&copy; 2019 <a href="https://github.com/scalaz/scalaz-schema">Scalaz-schema Maintainers</a></p>
        |""".stripMargin
    ),
    micrositeName := "Scalaz-Schema",
    micrositeDescription := "Abstract over data structures in a principled, purely functional way",
    micrositeAuthor := "Scalaz-schema contributors",
    micrositeOrganizationHomepage := "https://github.com/scalaz/scalaz-schema",
    micrositeGitterChannelUrl := "scalaz/scalaz-schema",
    micrositeGitHostingUrl := "https://github.com/scalaz/scalaz-schema",
    micrositeGithubOwner := "scalaz",
    micrositeGithubRepo := "scalaz-schema",
    micrositeFavicons := Seq(microsites.MicrositeFavicon("favicon.png", "512x512")),
    micrositeDocumentationUrl := "docs/schemas.html",
    micrositeDocumentationLabelDescription := "Documentation",
    micrositeBaseUrl := "/scalaz-schema",
    micrositePalette := Map(
      "brand-primary"   -> "#990000",
      "brand-secondary" -> "#000000",
      "brand-tertiary"  -> "#990000",
      "gray-dark"       -> "#453E46",
      "gray"            -> "#837F84",
      "gray-light"      -> "#E3E2E3",
      "gray-lighter"    -> "#F4F3F4",
      "white-color"     -> "#FFFFFF"
    )
  )
