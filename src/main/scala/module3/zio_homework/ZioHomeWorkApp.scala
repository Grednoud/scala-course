package module3.zio_homework

import zio._

object ZioHomeWorkApp extends ZIOAppDefault {

  /**
   * Главное приложение для домашнего задания
   * 
   * В ZIO 2:
   * - App заменен на ZIOAppDefault
   * - run возвращает ZIO[Any, Any, Any] вместо URIO[ZEnv, ExitCode]
   */

  override def run: ZIO[Any, Any, Any] = 
    echo.catchAll(err => Console.printLine(s"Error: $err"))

}
