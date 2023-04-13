ThisBuild / organization := "org.quasigroup"
ThisBuild / scalaVersion := "3.2.2"

val FS2Version = "3.6.1"
val LogbackVersion = "1.4.5"
val WeaverVersion = "0.8.1"
val KittenVersion = "3.0.0"
val CatsMTLVersion = "1.3.0"
val Log4CatsVersion = "2.5.0"
val CatsRetryVersion = "3.1.0"
val CirceVersion = "0.14.5"

lazy val root = (project in file(".")).settings(
  name := "scala-ibclient",
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-xml" % "2.1.0",
    "org.typelevel" %% "kittens" % KittenVersion,
    "org.typelevel" %% "cats-mtl" % CatsMTLVersion,
    "org.typelevel" %% "log4cats-slf4j" % Log4CatsVersion, // Direct Slf4j Support - Recommended
    "ch.qos.logback" % "logback-classic" % LogbackVersion,
    "com.github.cb372" %% "cats-retry" % CatsRetryVersion,
    "co.fs2" %% "fs2-core" % FS2Version,
    "co.fs2" %% "fs2-io" % FS2Version,
    "co.fs2" %% "fs2-scodec" % FS2Version,
    "org.typelevel" %% "shapeless3-deriving" % "3.3.0",
    "io.circe" %% "circe-core" % CirceVersion,
    "io.circe" %% "circe-generic" % CirceVersion,
    "io.circe" %% "circe-parser" % CirceVersion,
    "com.disneystreaming" %% "weaver-cats" % WeaverVersion % Test,
    "com.disneystreaming" %% "weaver-scalacheck" % WeaverVersion % Test
  ),
  testFrameworks += new TestFramework("weaver.framework.CatsEffect")
)
