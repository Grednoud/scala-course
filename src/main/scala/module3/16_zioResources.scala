package module3

import zio._

import java.io.{BufferedReader, FileReader, IOException}

object zioResources {

  /**
   * В ZIO 2 ZManaged заменен на Scope и ZIO.acquireRelease
   * 
   * Основные изменения:
   * - ZManaged[R, E, A] -> ZIO[R with Scope, E, A]
   * - ZManaged.make -> ZIO.acquireRelease
   * - use -> ZIO.scoped
   */

  // Пример: работа с файлом

  def openFile(name: String): ZIO[Any, IOException, BufferedReader] =
    ZIO.attemptBlockingIO(new BufferedReader(new FileReader(name)))

  def closeFile(reader: BufferedReader): UIO[Unit] =
    ZIO.succeed(reader.close())

  // Создание scoped ресурса
  def managedFile(name: String): ZIO[Scope, IOException, BufferedReader] =
    ZIO.acquireRelease(openFile(name))(closeFile)

  // Использование ресурса
  val useFile: ZIO[Any, IOException, String] = ZIO.scoped {
    for {
      reader <- managedFile("example.txt")
      line <- ZIO.attemptBlockingIO(reader.readLine())
    } yield line
  }

  // Пример: составной ресурс
  case class Connection(id: String)
  case class Session(connection: Connection, id: String)

  def acquireConnection: Task[Connection] = 
    ZIO.succeed(Connection("conn-1"))

  def releaseConnection(conn: Connection): UIO[Unit] = 
    Console.printLine(s"Releasing connection ${conn.id}").orDie

  def acquireSession(conn: Connection): Task[Session] = 
    ZIO.succeed(Session(conn, "session-1"))

  def releaseSession(session: Session): UIO[Unit] = 
    Console.printLine(s"Releasing session ${session.id}").orDie

  val scopedConnection: ZIO[Scope, Throwable, Connection] =
    ZIO.acquireRelease(acquireConnection)(releaseConnection)

  def scopedSession(conn: Connection): ZIO[Scope, Throwable, Session] =
    ZIO.acquireRelease(acquireSession(conn))(releaseSession)

  // Комбинирование ресурсов
  val combinedResources: ZIO[Any, Throwable, Session] = ZIO.scoped {
    for {
      conn <- scopedConnection
      session <- scopedSession(conn)
    } yield session
  }

  // Пример: finalizer
  val withFinalizer: ZIO[Any, Nothing, Unit] = ZIO.scoped {
    ZIO.addFinalizer(Console.printLine("Cleanup completed").orDie) *>
    Console.printLine("Doing work").orDie
  }

}
