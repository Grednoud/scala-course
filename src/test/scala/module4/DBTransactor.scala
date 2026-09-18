package module4

import zio._
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import com.dimafeng.testcontainers.PostgreSQLContainer
import io.getquill.{NamingStrategy, Escape, Literal}
import io.getquill.PostgresZioJdbcContext

object DBTransactor {

  /**
   * В ZIO 2 + Quill 4.x:
   * - Has[A] больше не используется  
   * - ZManaged -> Scope + ZIO.acquireRelease
   * - PostgresZioJdbcContext используется напрямую
   */

  type DataSource = javax.sql.DataSource

  object Ctx extends PostgresZioJdbcContext(NamingStrategy(Escape, Literal))

  def test: ZLayer[PostgreSQLContainer, Throwable, DataSource] = 
    ZLayer.scoped {
      for {
        pg <- ZIO.service[PostgreSQLContainer]
        config <- ZIO.attempt {
          val hc = new HikariConfig()
          hc.setUsername(pg.username)
          hc.setPassword(pg.password)
          hc.setJdbcUrl(pg.jdbcUrl)
          hc.setDriverClassName(pg.driverClassName)
          hc
        }
        ds <- ZIO.acquireRelease(
          ZIO.attempt(new HikariDataSource(config))
        )(ds => ZIO.succeed(ds.close()))
      } yield ds
    }

}
