package module4.phoneBook

import zio._
import liquibase.Liquibase
import liquibase.resource.ClassLoaderResourceAccessor
import liquibase.database.jvm.JdbcConnection
import io.getquill.{NamingStrategy, Escape, Literal, SnakeCase}
import io.getquill.PostgresZioJdbcContext
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import io.getquill.JdbcContextConfig
import io.getquill.util.LoadConfig
import module4.phoneBook.configuration.{Config, Configuration}

package object db {

  /**
   * В ZIO 2 + Quill 4.x:
   * - Используется PostgresZioJdbcContext
   * - ZManaged заменяется на Scope + ZIO.acquireRelease
   */

  type DataSource = javax.sql.DataSource

  object Ctx extends PostgresZioJdbcContext(NamingStrategy(Escape, Literal))

  def hikariDS: HikariDataSource = new JdbcContextConfig(LoadConfig("db")).dataSource

  val zioDS: ZLayer[Any, Throwable, DataSource] = ZLayer.scoped {
    ZIO.acquireRelease(ZIO.attempt(hikariDS))(ds => ZIO.succeed(ds.close()))
  }

  object LiquibaseService {

    type LiquibaseService = LiquibaseService.Service
    type Liqui = Liquibase

    trait Service {
      def performMigration: RIO[Liquibase, Unit]
    }

    class Impl extends Service {
      override def performMigration: RIO[Liquibase, Unit] = 
        ZIO.serviceWith[Liquibase](_.update("dev"))
    }
     
    def mkLiquibase(config: Config): ZIO[DataSource with Scope, Throwable, Liquibase] = 
      for {
        ds <- ZIO.service[DataSource]
        classLoader <- ZIO.attempt(classOf[LiquibaseService].getClassLoader)
        classLoaderAccessor <- ZIO.attempt(new ClassLoaderResourceAccessor(classLoader))
        jdbcConn <- ZIO.acquireRelease(
          ZIO.attempt(new JdbcConnection(ds.getConnection()))
        )(c => ZIO.succeed(c.close()))
        liqui <- ZIO.attempt(new Liquibase(config.liquibase.changeLog, classLoaderAccessor, jdbcConn))
      } yield liqui


    val liquibaseLayer: ZLayer[Configuration with DataSource, Throwable, Liquibase] = ZLayer.scoped {
      for {
        config <- ZIO.service[Config]
        liquibase <- mkLiquibase(config)
      } yield liquibase
    }


    def liquibase: URIO[Liquibase, Liquibase] = ZIO.service[Liquibase]

    val live: ULayer[LiquibaseService] = ZLayer.succeed(new Impl)

    def performMigration: RIO[Liquibase, Unit] = 
      ZIO.serviceWith[Liquibase](_.update("dev"))

  }
}
