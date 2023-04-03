ThisBuild / organization := "org.quasigroup"
ThisBuild / scalaVersion := "3.2.0"

val FS2Version = "3.6.1"
val LogbackVersion = "1.4.5"
val WeaverVersion = "0.8.1"
val KittenVersion = "3.0.0"
val CirisVersion = "3.1.0"
val CatsMTLVersion = "1.3.0"
val Log4CatsVersion = "2.5.0"
val CatsRetryVersion = "3.1.0"
val RefinedVersion = "0.10.2"
val CirceVersion = "0.14.5"
val PureConfigVersion = "0.17.2"

lazy val root = (project in file(".")).settings(
    name := "scala-bench-ta-stock",
    //compileOrder := CompileOrder.JavaThenScala,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "kittens" % KittenVersion,
      "org.typelevel" %% "cats-mtl" % CatsMTLVersion,
      "org.typelevel" %% "log4cats-slf4j" % Log4CatsVersion, // Direct Slf4j Support - Recommended
      "ch.qos.logback" % "logback-classic" % LogbackVersion,
      "is.cir" %% "ciris" % CirisVersion,
      "is.cir" %% "ciris-refined" % CirisVersion,
      "is.cir" %% "ciris-http4s" % CirisVersion,
      "com.github.pureconfig" %% "pureconfig-core" % PureConfigVersion,
//      "com.github.pureconfig" %% "pureconfig-http4s" % PureConfigVersion,
//      "com.github.pureconfig" %% "pureconfig-cats-effect" % PureConfigVersion,
      "com.github.cb372" %% "cats-retry" % CatsRetryVersion,
      "eu.timepit" %% "refined" % RefinedVersion,
      "eu.timepit" %% "refined-cats" % RefinedVersion,
      "io.circe" %% "circe-refined" % CirceVersion,
      "co.fs2"%% "fs2-core" % FS2Version,
      "co.fs2"%% "fs2-io" % FS2Version,
      "co.fs2"%% "fs2-scodec" % FS2Version,
      "org.scala-lang.modules" %% "scala-parser-combinators"% "2.2.0",
      "org.typelevel" %% "shapeless3-deriving" % "3.3.0",
      "com.disneystreaming" %% "weaver-cats" % WeaverVersion % Test,
      "com.disneystreaming" %% "weaver-scalacheck" % WeaverVersion % Test
    ),
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
  )
