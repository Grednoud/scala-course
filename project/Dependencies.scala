import sbt.ModuleID
import sbt._

object Dependencies {

  lazy val ScalaVersion = "2.13.18"

  lazy val ZioVersion = "2.1.26"
  lazy val ZioConfigVersion = "4.0.7"
  lazy val ZioInteropCatsVersion = "23.1.0.13"

  lazy val Http4sVersion = "0.23.37"
  lazy val CirceVersion = "0.14.16"

  lazy val QuillVersion = "4.8.5"
  lazy val PostgresVersion = "42.7.4"
  lazy val HikariVersion = "6.2.1"

  lazy val LiquibaseVersion = "4.30.0"
  lazy val LogbackVersion = "1.5.18"

  lazy val TestContainersVersion = "0.44.1"
  lazy val ScalaTestVersion = "3.2.19"
  lazy val MockitoVersion = "5.18.0"

  lazy val zio: Seq[ModuleID] = Seq(
    "dev.zio" %% "zio"          % ZioVersion,
    "dev.zio" %% "zio-streams"  % ZioVersion,
    "dev.zio" %% "zio-test"     % ZioVersion % Test,
    "dev.zio" %% "zio-test-sbt" % ZioVersion % Test
  )

  lazy val zioConfig: Seq[ModuleID] = Seq(
    "dev.zio" %% "zio-config"          % ZioConfigVersion,
    "dev.zio" %% "zio-config-magnolia" % ZioConfigVersion,
    "dev.zio" %% "zio-config-typesafe" % ZioConfigVersion
  )

  lazy val zioInteropCats: Seq[ModuleID] = Seq(
    "dev.zio" %% "zio-interop-cats" % ZioInteropCatsVersion
  )

  lazy val http4sServer: Seq[ModuleID] = Seq(
    "org.http4s" %% "http4s-dsl"          % Http4sVersion,
    "org.http4s" %% "http4s-circe"        % Http4sVersion,
    "org.http4s" %% "http4s-ember-server" % Http4sVersion,
    "org.http4s" %% "http4s-ember-client" % Http4sVersion
  )

  lazy val circe: Seq[ModuleID] = Seq(
    "io.circe" %% "circe-generic" % CirceVersion,
    "io.circe" %% "circe-parser"  % CirceVersion
  )

  lazy val quill: Seq[ModuleID] = Seq(
    "io.getquill" %% "quill-jdbc-zio" % QuillVersion
  )

  lazy val database: Seq[ModuleID] = Seq(
    "org.postgresql" % "postgresql"     % PostgresVersion,
    "com.zaxxer"     % "HikariCP"       % HikariVersion
  )

  lazy val liquibase: ModuleID = "org.liquibase" % "liquibase-core" % LiquibaseVersion

  lazy val logback: ModuleID = "ch.qos.logback" % "logback-classic" % LogbackVersion

  lazy val testContainers: Seq[ModuleID] = Seq(
    "com.dimafeng" %% "testcontainers-scala-postgresql" % TestContainersVersion % Test,
    "com.dimafeng" %% "testcontainers-scala-scalatest"  % TestContainersVersion % Test
  )

  lazy val testing: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest"    % ScalaTestVersion % Test,
    "org.mockito"    % "mockito-core" % MockitoVersion   % Test
  )
}
