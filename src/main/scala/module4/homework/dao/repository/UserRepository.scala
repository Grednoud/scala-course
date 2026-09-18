package module4.homework.dao.repository

import zio._
import module4.homework.dao.entity.{User, Role, UserToRole, UserId, RoleCode}
import module4.phoneBook.db


object UserRepository {

    /**
     * В ZIO 2 + Quill 4.x:
     * - Используется PostgresZioJdbcContext
     */

    import db.Ctx._

    type UserRepository = Service

    trait Service {
        def findUser(userId: UserId): ZIO[db.DataSource, Throwable, Option[User]]
        def createUser(user: User): ZIO[db.DataSource, Throwable, User]
        def createUsers(users: List[User]): ZIO[db.DataSource, Throwable, List[User]]
        def updateUser(user: User): ZIO[db.DataSource, Throwable, Unit]
        def deleteUser(user: User): ZIO[db.DataSource, Throwable, Unit]
        def findByLastName(lastName: String): ZIO[db.DataSource, Throwable, List[User]]
        def list(): ZIO[db.DataSource, Throwable, List[User]]
        def userRoles(userId: UserId): ZIO[db.DataSource, Throwable, List[Role]]
        def insertRoleToUser(roleCode: RoleCode, userId: UserId): ZIO[db.DataSource, Throwable, Unit]
        def listUsersWithRole(roleCode: RoleCode): ZIO[db.DataSource, Throwable, List[User]]
        def findRoleByCode(roleCode: RoleCode): ZIO[db.DataSource, Throwable, Option[Role]]
    }

    class ServiceImpl extends Service {

        val userSchema = quote {
            querySchema[User]("users")
        }

        val roleSchema = quote {
            querySchema[Role]("roles")
        }

        val userToRoleSchema = quote {
            querySchema[UserToRole]("user_roles")
        }

        def findUser(userId: UserId): ZIO[db.DataSource, Throwable, Option[User]] = 
            run(userSchema.filter(_.id == lift(userId.value))).map(_.headOption)
        
        def createUser(user: User): ZIO[db.DataSource, Throwable, User] = 
            run(userSchema.insertValue(lift(user))).as(user)
        
        def createUsers(users: List[User]): ZIO[db.DataSource, Throwable, List[User]] = 
            run(liftQuery(users).foreach(u => userSchema.insertValue(u))).as(users)
        
        def updateUser(user: User): ZIO[db.DataSource, Throwable, Unit] = 
            run(userSchema.filter(_.id == lift(user.id)).updateValue(lift(user))).unit
        
        def deleteUser(user: User): ZIO[db.DataSource, Throwable, Unit] = 
            run(userSchema.filter(_.id == lift(user.id)).delete).unit
        
        def findByLastName(lastName: String): ZIO[db.DataSource, Throwable, List[User]] = 
            run(userSchema.filter(_.lastName == lift(lastName)))
        
        def list(): ZIO[db.DataSource, Throwable, List[User]] = 
            run(userSchema)
        
        def userRoles(userId: UserId): ZIO[db.DataSource, Throwable, List[Role]] = 
            run(
                for {
                    ur <- userToRoleSchema if ur.userId == lift(userId.value)
                    r <- roleSchema if r.code == ur.roleCode
                } yield r
            )
        
        def insertRoleToUser(roleCode: RoleCode, userId: UserId): ZIO[db.DataSource, Throwable, Unit] = 
            run(userToRoleSchema.insertValue(lift(UserToRole(userId.value, roleCode.value)))).unit
        
        def listUsersWithRole(roleCode: RoleCode): ZIO[db.DataSource, Throwable, List[User]] = 
            run(
                for {
                    ur <- userToRoleSchema if ur.roleCode == lift(roleCode.value)
                    u <- userSchema if u.id == ur.userId
                } yield u
            )
        
        def findRoleByCode(roleCode: RoleCode): ZIO[db.DataSource, Throwable, Option[Role]] = 
            run(roleSchema.filter(_.code == lift(roleCode.value))).map(_.headOption)
                
    }

    val live: ULayer[UserRepository] = ZLayer.succeed(new ServiceImpl)
}
