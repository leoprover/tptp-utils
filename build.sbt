lazy val commonSettings = Seq(
  organization := "org.leo",
  scalaVersion := "2.13.8",
  scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
    ),
  version := "1.2.1",
  licenses += "BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"),
  libraryDependencies += "io.github.leoprover" %% "scala-tptp-parser" % "1.6.4",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"
)


lazy val tptpUtils = (project in file("."))
  .disablePlugins(sbtassembly.AssemblyPlugin)
  .settings(
    commonSettings,
    name := "tptp-utils",
    description := "A library and tool for some TPTP utility functions."
  ).aggregate(tptpUtilsRuntime, tptpUtilsApp)

lazy val tptpUtilsRuntime = (project in file("tptp-utils-runtime"))
	.settings(
	  commonSettings,
    name := "tptp-utils-runtime",
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false),
    test in assembly := {},
    assemblyJarName in assembly := s"${name.value}-${version.value}.jar",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.1.0",
	)

lazy val tptpUtilsApp = (project in file("tptp-utils-app"))
	.settings(
	  commonSettings,
    name := "tptp-utils-app",
    Compile/mainClass := Some("leo.modules.TPTPUtilsApp"),
    mainClass in assembly := Some("leo.modules.TPTPUtilsApp"),
    test in assembly := {},
    assemblyJarName in assembly := s"${name.value}-${version.value}.jar",
	).dependsOn(tptpUtilsRuntime)
