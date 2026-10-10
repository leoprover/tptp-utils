val scala2 = "2.13.18"
val scala3 = "3.9.0"

lazy val commonSettings = Seq(
  organization := "io.github.leoprover",
  homepage := Some(url("https://github.com/leoprover/tptp-utils")),
  scmInfo := Some(ScmInfo(
      browseUrl = url("https://github.com/leoprover/tptp-utils"),
      connection = "scm:git:git@github.com:leoprover/tptp-utils.git"
    )),
    
  developers := List(
      Developer(
        "lex-lex",
        "Alexander Steen",
        "alx.steen@gmail.com",
        url("https://www.alexandersteen.de/")
      )
    ),
  licenses += "BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"),
  
  crossScalaVersions := Seq(scala3) ++ Seq(scala2),
  scalaVersion := crossScalaVersions.value.head,
  scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
    ),
  // Version number explicitly removed as this is handled by the release plugin
  
  libraryDependencies += "io.github.leoprover" %% "scala-tptp-parser" % "1.7.5",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % "test"
)


lazy val tptpUtils = (project in file("."))
  .disablePlugins(sbtassembly.AssemblyPlugin)
  .settings(
    commonSettings,
    name := "tptp-utils",
    description := "A library and tool for some TPTP utility functions.",
    publish / skip := true // don't publish the root project
  ).aggregate(tptpUtilsRuntime, tptpUtilsApp)

lazy val tptpUtilsRuntime = (project in file("tptp-utils-runtime"))
	.settings(
	  commonSettings,
    name := "tptp-utils-runtime",
    description := """tptp-utils-runtime is a library for pre- and post-processing automated theorem 
                     | proving input files (so-called problem files) given in the TPTP 
                     | (Thousands of Problems for Theorem Proving) syntax standard. """.stripMargin,
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
    nativeImageOptions += s"-H:ReflectionConfigurationFiles=${baseDirectory.value / ".." / "contrib" / "native-image-configs" / "reflect-config.json"}",
    nativeImageOptions += s"-H:ConfigurationFileDirectories=${baseDirectory.value / ".." / "contrib" / "native-image-configs" }",
    nativeImageOptions +="-H:+JNI",
    assembly / test := {},
    assembly / assemblyJarName := s"${name.value}-${version.value}.jar",
    publish / skip := true // don't publish the root project
	).dependsOn(tptpUtilsRuntime)
	.enablePlugins(NativeImagePlugin)
