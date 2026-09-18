package module3.userDAO

import zio._
import module3.userService.{User, UserID}

object UserDAOMock {

  /**
   * В ZIO 2:
   * - @mockable макрос больше не доступен
   * - Моки создаются вручную через ZLayer
   */

  def make(findByResult: UserID => Option[User]): ULayer[UserDAO] = 
    ZLayer.succeed(new UserDAO.Service {
      def list(): Task[List[User]] = ZIO.succeed(List.empty)
      def findBy(id: UserID): Task[Option[User]] = ZIO.succeed(findByResult(id))
    })

}
