package module4

import zio._
import com.dimafeng.testcontainers.PostgreSQLContainer

object TestContainer {

  /**
   * В ZIO 2:
   * - Has[A] больше не используется
   * - ZManaged -> Scope + ZIO.acquireRelease
   * - effectBlocking -> ZIO.attemptBlocking
   */

  type Postgres = PostgreSQLContainer
  
  def postgres(): ZLayer[Any, Nothing, Postgres] =
    ZLayer.scoped {
      ZIO.acquireRelease(
        ZIO.attemptBlocking {
          val container = new PostgreSQLContainer()
          container.start()
          container
        }.orDie
      )(container => ZIO.attemptBlocking(container.stop()).orDie)
    }
}
