package module4.phoneBook

import zio._

object Main extends ZIOAppDefault {

  /**
   * В ZIO 2:
   * - App заменен на ZIOAppDefault
   * - run возвращает ZIO[Any, Any, Any]
   */

  override def run: ZIO[Any, Any, Any] = 
    Server.server
      .provide(Server.appEnvironment)
}
