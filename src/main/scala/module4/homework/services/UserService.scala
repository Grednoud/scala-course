package module4.homework.services

import zio._
import module4.homework.dao.entity.{User, UserId}
import module4.homework.dao.repository.UserRepository
import module4.phoneBook.db

object UserService {

    /**
     * В ZIO 2:
     * - Has[A] больше не используется
     */

    type UserService = Service

    trait Service {
        def getUser(userId: UserId): ZIO[db.DataSource, Throwable, Option[User]]
        def listUsers: ZIO[db.DataSource, Throwable, List[User]]
        def createUser(user: User): ZIO[db.DataSource, Throwable, User]
    }

    class ServiceImpl(userRepository: UserRepository.Service) extends Service {
        def getUser(userId: UserId): ZIO[db.DataSource, Throwable, Option[User]] = 
            userRepository.findUser(userId)
        
        def listUsers: ZIO[db.DataSource, Throwable, List[User]] = 
            userRepository.list()
        
        def createUser(user: User): ZIO[db.DataSource, Throwable, User] = 
            userRepository.createUser(user)
    }

    val live: ZLayer[UserRepository.Service, Nothing, UserService] = 
        ZLayer.fromFunction((repo: UserRepository.Service) => new ServiceImpl(repo): UserService)

    def getUser(userId: UserId): ZIO[UserService with db.DataSource, Throwable, Option[User]] =
        ZIO.serviceWithZIO[UserService](_.getUser(userId))

    def listUsers: ZIO[UserService with db.DataSource, Throwable, List[User]] =
        ZIO.serviceWithZIO[UserService](_.listUsers)

    def createUser(user: User): ZIO[UserService with db.DataSource, Throwable, User] =
        ZIO.serviceWithZIO[UserService](_.createUser(user))

}
