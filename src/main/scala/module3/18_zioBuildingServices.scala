package module3


import zio._
import module3.userService.UserService
import module3.userService.UserID
import module3.emailService.EmailService
import module3.userDAO.UserDAO

object buildingZIOServices{

  /**
   * В ZIO 2:
   * - Has[A] больше не используется, сервисы указываются напрямую в типах
   * - ZLayer создается через ZLayer.fromFunction или ZLayer.fromZIO
   * - Композиция слоев через ++ (horizontal) и >>> (vertical)
   */

  lazy val app: ZIO[UserService with EmailService, Throwable, Unit] = 
    UserService.notifyUser(UserID(1))

  lazy val appEnv: ZLayer[Any, Nothing, UserService with EmailService] = 
    UserDAO.live >>> UserService.live ++ EmailService.live

  

  def main(args: Array[String]): Unit = {
     Unsafe.unsafe { implicit unsafe =>
       Runtime.default.unsafe.run(
         app.provide(appEnv)
       ).getOrThrowFiberFailure()
     }
  }

}
