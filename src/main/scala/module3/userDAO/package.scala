package module3

import zio._
import userService.{User, UserID}

package object userDAO {

    /**
     * В ZIO 2:
     * - Has[A] заменяется на просто A в R-типе
     * - @accessible макрос заменен на ручные accessor методы или ZIO.serviceWith
     */

    type UserDAO = UserDAO.Service
    
    object UserDAO {
        trait Service {
            def list(): Task[List[User]]
            def findBy(id: UserID): Task[Option[User]]
        }

        val live: ULayer[UserDAO] = ZLayer.succeed(
          new Service {
            def list(): Task[List[User]] = ZIO.succeed(List.empty)
            def findBy(id: UserID): Task[Option[User]] = ZIO.succeed(None)
          }
        )

        def list(): RIO[UserDAO, List[User]] = 
          ZIO.serviceWithZIO[UserDAO](_.list())

        def findBy(id: UserID): RIO[UserDAO, Option[User]] = 
          ZIO.serviceWithZIO[UserDAO](_.findBy(id))
    }

  
}
