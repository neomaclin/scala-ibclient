ThisBuild / organization := "org.quasigroup"
ThisBuild / scalaVersion := "2.13.10"

val Http4sVersion = "0.23.18"
val LogbackVersion = "1.4.5"
val WeaverVersion = "0.8.1"
val DoobieVersion = "1.0.0-RC2"
val KittenVersion = "3.0.0"
val CirisVersion = "3.1.0"
val CatsMTLVersion = "1.3.0"
val Log4CatsVersion = "2.5.0"
val CatsRetryVersion = "3.1.0"
val RefinedVersion = "0.10.1"
val CirceVersion = "0.14.4"
val DeclineVersion = "2.4.1"
val TsecVersion = "0.4.0"
val PureConfigVersion = "0.17.2"
val FS2BlobStoreVersion = "0.9.7"
val Redis4catsVersion = "1.4.0"
//val QuillVersion = "4.6.0"

lazy val root = (project in file(".")).settings(
    name := "scala-bench-ta-stock",
    compileOrder := CompileOrder.JavaThenScala,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "kittens" % KittenVersion,
      "org.typelevel" %% "cats-mtl" % CatsMTLVersion,
      "org.typelevel" %% "log4cats-slf4j" % Log4CatsVersion, // Direct Slf4j Support - Recommended
      "ch.qos.logback" % "logback-classic" % LogbackVersion,
      "is.cir" %% "ciris" % CirisVersion,
      "is.cir" %% "ciris-refined" % CirisVersion,
      "is.cir" %% "ciris-http4s" % CirisVersion,
      "com.github.pureconfig" %% "pureconfig-core" % PureConfigVersion,
      "com.github.pureconfig" %% "pureconfig-generic" % PureConfigVersion,
      "com.github.pureconfig" %% "pureconfig-http4s" % PureConfigVersion,
      "com.github.pureconfig" %% "pureconfig-cats-effect" % PureConfigVersion,
      "com.github.cb372" %% "cats-retry" % CatsRetryVersion,
//      "eu.timepit" %% "refined" % RefinedVersion,
//      "eu.timepit" %% "refined-cats" % RefinedVersion,
//      "io.circe" %% "circe-refined" % CirceVersion,
//      "com.monovore" %% "decline" % DeclineVersion,
//      "com.monovore" %% "decline-refined" % DeclineVersion,
//      "org.tpolecat" %% "doobie-core" % DoobieVersion,
//      "org.tpolecat" %% "doobie-refined" % DoobieVersion,
//      "org.tpolecat" %% "doobie-h2" % DoobieVersion,
//      "org.tpolecat" %% "doobie-hikari" % DoobieVersion,
//      "org.tpolecat" %% "doobie-postgres" % DoobieVersion,
//      "org.tpolecat" %% "doobie-postgres-circe" % DoobieVersion,
//      "io.github.jmcardon" %% "tsec-http4s" % TsecVersion,
//      "io.github.jmcardon" %% "tsec-password" % TsecVersion,
//      "dev.profunktor" %% "redis4cats-effects" % Redis4catsVersion,
//      "dev.profunktor" %% "redis4cats-log4cats" % Redis4catsVersion,

      "org.scala-lang.modules" %% "scala-parser-combinators"% "2.2.0",
      "com.disneystreaming" %% "weaver-cats" % WeaverVersion % Test,
    ),
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  )
