lazy val commonSettings = Seq(
  organization := "org.leo",
  scalaVersion := "2.13.12",
  scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
    ),
  version := "1.3.0",
  licenses += "BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"),
  libraryDependencies += "io.github.leoprover" %% "scala-tptp-parser" % "1.7.3",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
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
    assembly / assemblyOption ~= {
      _.withIncludeScala(false)
    },
    assembly / test := {},
    assembly / assemblyJarName := s"${name.value}-${version.value}.jar",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
	)

lazy val tptpUtilsApp = (project in file("tptp-utils-app"))
	.settings(
	  commonSettings,
    name := "tptp-utils-app",
    Compile / mainClass := Some("leo.modules.TPTPUtilsApp"),
    assembly / mainClass := Some("leo.modules.TPTPUtilsApp"),
    assembly / test := {},
    assembly / assemblyJarName := s"${name.value}-${version.value}.jar",
	).dependsOn(tptpUtilsRuntime)
