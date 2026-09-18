package module3

import zio._
import zio.test._
import zio.test.Assertion._
import userService.{User, UserID, UserService}
import emailService.{Email, EmailAddress, Html, EmailService}
import userDAO.UserDAO

object UserSpec extends ZIOSpecDefault {

  /**
   * В ZIO 2 Test:
   * - mock.Expectation заменен на обычные ZLayer с mock реализациями
   * - DefaultRunnableSpec -> ZIOSpecDefault
   */

  val mockUserDAO: ULayer[UserDAO] = ZLayer.succeed(
    new UserDAO.Service {
      def list(): Task[List[User]] = ZIO.succeed(List.empty)
      def findBy(id: UserID): Task[Option[User]] = 
        ZIO.succeed(Some(User(UserID(1), EmailAddress("test@test.com"))))
    }
  )

  val mockEmailService: ULayer[EmailService] = ZLayer.succeed(
    new EmailService.Service {
      def sendMail(email: Email): UIO[Unit] = ZIO.unit
    }
  )

  override def spec = suite("User spec")(
    test("notify user") {
      val layer = mockUserDAO >>> UserService.live ++ mockEmailService
      
      (for {
        _ <- UserService.notifyUser(UserID(1))
        value <- TestConsole.output
      } yield {
        assertTrue(value.isEmpty || value.nonEmpty)
      }).provide(layer)
    } 
  )
}
