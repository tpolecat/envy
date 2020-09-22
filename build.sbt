
// Global Settings
lazy val commonSettings = Seq(

  // Resolvers
  resolvers += Resolver.sonatypeRepo("public"),
  resolvers += Resolver.sonatypeRepo("snapshots"),

  // Publishing
  organization := "org.tpolecat",
  licenses    ++= Seq(("MIT", url("http://opensource.org/licenses/MIT"))),
  homepage     := Some(url("https://github.com/tpolecat/envy")),
  developers   := List(
    Developer("tpolecat", "Rob Norris", "rob_norris@mac.com", url("http://www.tpolecat.org"))
  ),

  // Headers
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2020 by Rob Norris
       |This software is licensed under the MIT License (MIT).
       |For more information see LICENSE or https://opensource.org/licenses/MIT
       |""".stripMargin
    )
  ),

  // Compilation
  scalaVersion       := "2.13.3",
  crossScalaVersions := Seq("2.12.11", scalaVersion.value),
  scalacOptions -= "-language:experimental.macros", // doesn't work cross-version
  Compile / doc     / scalacOptions --= Seq("-Xfatal-warnings"),
  Compile / doc     / scalacOptions ++= Seq(
    "-groups",
    "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
    "-doc-source-url", "https://github.com/tpolecat/envy/blob/v" + version.value + "€{FILE_PATH}.scala",
  ),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),

  // Coverage Exclusions
  coverageExcludedPackages := "",

)

lazy val envy = project
  .in(file("."))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(publish / skip := true)
  .dependsOn(core)
  .aggregate(core)

lazy val core = project
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(
    name := "envy-core",
    description := "Tiny environment-based configuration loader with good diagnostics.",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"    % "2.1.1",
      "org.typelevel" %% "cats-effect"  % "2.1.4",
    )
  )

lazy val docs = project
  .in(file("modules/docs"))
  .dependsOn(core)
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(ParadoxPlugin)
  .enablePlugins(ParadoxSitePlugin)
  .enablePlugins(GhpagesPlugin)
  .enablePlugins(MdocPlugin)
  .settings(commonSettings)
  .settings(
    scalacOptions      := Nil,
    git.remoteRepo     := "git@github.com:tpolecat/envy.git",
    ghpagesNoJekyll    := true,
    publish / skip     := true,
    paradoxTheme       := Some(builtinParadoxTheme("generic")),
    version            := version.value.takeWhile(_ != '+'), // strip off the +3-f22dca22+20191110-1520-SNAPSHOT business
    paradoxProperties ++= Map(
      "scala-versions"          -> (crossScalaVersions in core).value.map(CrossVersion.partialVersion).flatten.map(_._2).mkString("2.", "/", ""),
      "org"                     -> organization.value,
      "scala.binary.version"    -> s"2.${CrossVersion.partialVersion(scalaVersion.value).get._2}",
      "core-dep"                -> s"${(core / name).value}_2.${CrossVersion.partialVersion(scalaVersion.value).get._2}",
      "version"                 -> version.value,
      "scaladoc.envy.base_url" -> s"https://static.javadoc.io/org.tpolecat/envy-core_2.13/${version.value}",
    ),
    mdocIn := (baseDirectory.value) / "src" / "main" / "paradox",
    Compile / paradox / sourceDirectory := mdocOut.value,
    makeSite := makeSite.dependsOn(mdoc.toTask("")).value,
    mdocExtraArguments := Seq("--no-link-hygiene"), // paradox handles this
)
