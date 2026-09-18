package module4

import zio._
import zio.test._
import liquibase.Liquibase
import liquibase.resource.ClassLoaderResourceAccessor
import liquibase.database.jvm.JdbcConnection

object MigrationAspects {

  /**
   * В ZIO 2:
   * - Has[A] больше не используется
   * - ZManaged -> Scope + ZIO.acquireRelease
   * - TestAspect.before для запуска миграций
   */
  
  def migrate(): TestAspect[Nothing, LiquibaseService.Liqui, Nothing, Any] = 
    TestAspect.before(LiquibaseService.performMigration.orDie)

}

object LiquibaseService {

    type Liqui = Liquibase
    type LiquibaseService = Service
    type DataSource = javax.sql.DataSource

    trait Service {
      def performMigration: RIO[Liquibase, Unit]
    }

    class Impl extends Service {
      override def performMigration: RIO[Liquibase, Unit] = 
        ZIO.serviceWith[Liquibase](_.update("dev"))
    }

    def mkLiquibase(): ZIO[DataSource with Scope, Throwable, Liquibase] = for {
      ds <- ZIO.service[DataSource]
      classLoader <- ZIO.attempt(classOf[LiquibaseService].getClassLoader)
      classLoaderAccessor <- ZIO.attempt(new ClassLoaderResourceAccessor(classLoader))
      jdbcConn <- ZIO.acquireRelease(
        ZIO.attempt(new JdbcConnection(ds.getConnection()))
      )(c => ZIO.succeed(c.close()))
      liqui <- ZIO.attempt(new Liquibase("src/test/resources/liquibase/main.xml", classLoaderAccessor, jdbcConn))
    } yield liqui

    val liquibaseLayer: ZLayer[DataSource, Throwable, Liquibase] = 
      ZLayer.scoped(mkLiquibase())

    def liquibase: URIO[Liquibase, Liquibase] = ZIO.service[Liquibase]

    val live: ULayer[LiquibaseService] = ZLayer.succeed(new Impl)

    def performMigration: RIO[Liquibase, Unit] = 
      ZIO.serviceWith[Liquibase](_.update("dev"))

}
