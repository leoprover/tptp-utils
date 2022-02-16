lazy val commonSettings = Seq(
  organization := "org.leo",
  scalaVersion := "2.13.8",
  scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
    ),
  licenses += "BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause")
)


lazy val tptpUtils = (project in file("."))
  .disablePlugins(sbtassembly.AssemblyPlugin)
  .settings(
    commonSettings,
    version := "1.1",
    name := "tptp-utils",
    description := "A library and tool for some TPTP utility functions."
  ).aggregate(tptpUtilsRuntime, tptpUtilsApp)

lazy val tptpUtilsRuntime = (project in file("tptp-utils-runtime"))
	.settings(
	  commonSettings,
    name := "tptp-utils-runtime",
    version := "1.1",
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false),
    test in assembly := {},
    assemblyJarName in assembly := s"${name.value}-${version.value}.jar",
    libraryDependencies += "io.github.leoprover" %% "scala-tptp-parser" % "1.6",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
	)

lazy val tptpUtilsApp = (project in file("tptp-utils-app"))
	.settings(
	  commonSettings,
    name := "tptp-utils-app",
    version := "1.1",
    Compile/mainClass := Some("leo.modules.TPTPUtilsApp"),
    mainClass in assembly := Some("leo.modules.TPTPUtilsApp"),
    test in assembly := {},
    assemblyJarName in assembly := s"${name.value}-${version.value}.jar",
    libraryDependencies += "io.github.leoprover" %% "scala-tptp-parser" % "1.6",
	  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
	).dependsOn(tptpUtilsRuntime)
