import Dependencies._

lazy val root = (project in file("."))
  .settings(
    name := "scala-dev-mooc-2024",
    version := "0.2",
    scalaVersion := Dependencies.ScalaVersion,

    libraryDependencies ++= Dependencies.zio,
    libraryDependencies ++= Dependencies.zioConfig,
    libraryDependencies ++= Dependencies.zioInteropCats,
    libraryDependencies ++= Dependencies.http4sServer,
    libraryDependencies ++= Dependencies.circe,
    libraryDependencies ++= Dependencies.quill,
    libraryDependencies ++= Dependencies.database,
    libraryDependencies ++= Dependencies.testContainers,
    libraryDependencies ++= Dependencies.testing,
    libraryDependencies ++= Seq(
      Dependencies.liquibase,
      Dependencies.logback
    ),
    libraryDependencies += "org.scala-lang" % "scala-reflect" % Dependencies.ScalaVersion,

    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),

    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-Ymacro-annotations"
    )
  )
