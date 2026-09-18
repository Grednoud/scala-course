package module3

import zio._

import scala.language.postfixOps

object di {

  type Query[_]
  type DBError
  type QueryResult[_]
  type Email
  type User


  trait DBService{
    def tx[T](query: Query[T]): IO[DBError, QueryResult[T]]
  }

  trait EmailService{
    def makeEmail(email: String, body: String): Task[Email]
    def sendEmail(email: Email): Task[Unit]
  }

  trait LoggingService{
    def log(str: String): Task[Unit]
  }

  trait UserService{
      def getUserBy(id: Int): RIO[LoggingService, User]
  }



  /**
   * Написать эффект который напечатет в консоль приветствие, подождет 5 секунд,
   * сгенерит рандомное число, напечатает его в консоль
   *   Console
   *   Clock
   *   Random
   * 
   * В ZIO 2 стандартные сервисы (Console, Clock, Random) больше не требуют Has[]
   * и доступны через компаньон-объекты напрямую
   */
  

  def e1: ZIO[UserService with LoggingService, Nothing, Unit] = for{
    userSerivce <- ZIO.service[UserService]
    _ <- userSerivce.getUserBy(1).orDie
    _ <- Console.printLine("Hello").orDie
    _ <- Clock.sleep(5.seconds)
    int <- Random.nextInt
    _ <- Console.printLine(int.toString()).orDie
  } yield ()


  def e2: ZIO[Any, Nothing, Unit] = for{
    _ <- Console.printLine("Hello").orDie
    _ <- Clock.sleep(5.seconds)
    int <- Random.nextInt
    _ <- Console.printLine(int.toString()).orDie
  } yield ()

  lazy val getUser: RIO[UserService with LoggingService, User] = 
    ZIO.serviceWithZIO[UserService](_.getUserBy(1).orDie)

  lazy val sendMail: ZIO[EmailService, Throwable, Unit] = 
    ZIO.serviceWithZIO[EmailService](_.makeEmail("", "").flatMap(_.sendEmail))


  /**
   * Эффект, который будет комбинацией двух эффектов выше
   */
  lazy val combined2: ZIO[UserService with EmailService with LoggingService, Throwable, (User, Unit)] = 
    for {
      user <- getUser
      unit <- sendMail
    } yield (user, unit)


  /**
   * Написать ZIO программу которая выполнит запрос и отправит email
   */
  val queryAndNotify: ZIO[UserService with EmailService with LoggingService, Throwable, Unit] = for{
    userService <- ZIO.service[UserService]
    emailService <- ZIO.service[EmailService]
    user <- userService.getUserBy(1)
    email <- emailService.makeEmail("", "")
    _ <- emailService.sendEmail(email)
  } yield ()



  lazy val services: UserService with EmailService with LoggingService = ???

  lazy val dBService: DBService = ???
  lazy val userService: UserService = ???

  lazy val emailService2: EmailService = ???

  def f(userService: UserService): UserService with EmailService with LoggingService = ???

  // provide - теперь работает через ZLayer
  // В ZIO 2 используется provideLayer вместо provide с простыми значениями


  lazy val servicesLayer: ZLayer[Any, Nothing, DBService with EmailService] = ???

  lazy val dbServiceLayer: ZLayer[Any, Nothing, DBService] = ???

  // provide layer
  lazy val e6 = ???

  // provide some layer
  lazy val e7 = ???

  // Вспомогательное расширение для EmailService
  implicit class EmailServiceOps(email: Email) {
    def sendEmail: ZIO[EmailService, Throwable, Unit] = 
      ZIO.serviceWithZIO[EmailService](_.sendEmail(email))
  }

}
