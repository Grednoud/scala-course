package module3

import zio._
import emailService.Email
import emailService.EmailAddress
import emailService.Html
import module3.emailService.EmailService
import module3.userDAO.UserDAO

package object userService {

    case class UserID(id: Int)
    case class User(id: UserID, email: EmailAddress)

    /**
     * В ZIO 2:
     * - Has[A] больше не используется
     * - Сервисы объявляются как trait и предоставляются через ZLayer
     */

    type UserService = UserService.Service

    object UserService {

      trait Service {
          def notifyUser(userId: UserID): RIO[EmailService, Unit]
      }

      class ServiceImpl(userDAO: UserDAO.Service) extends Service {
          def notifyUser(userId: UserID): RIO[EmailService, Unit] = 
              for {
                user <- userDAO.findBy(userId).someOrFail(new Throwable("User not found"))
                email = Email(user.email, Html("Hello here"))
                _ <- EmailService.sendMail(email)
              } yield ()
      }

      val live: ZLayer[UserDAO, Nothing, UserService] = 
        ZLayer.fromFunction((dao: UserDAO) => new ServiceImpl(dao): UserService)

      def notifyUser(userId: UserID): ZIO[UserService with EmailService, Throwable, Unit] =
        ZIO.serviceWithZIO[UserService](_.notifyUser(userId))

    } 

}
