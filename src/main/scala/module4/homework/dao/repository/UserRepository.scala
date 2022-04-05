package module4.homework.dao.repository

import zio.Has

import io.getquill.context.ZioJdbc.QIO
import module4.homework.dao.entity.User
import zio.macros.accessible
import zio.{ULayer, ZLayer}
import module4.homework.dao.entity.{Role, UserToRole}
import module4.homework.dao.entity.UserId
import module4.homework.dao.entity.RoleCode
import module4.phoneBook.db

object UserRepository{


    val dc = db.Ctx
    import dc._

    type UserRepository = Has[Service]

    trait Service{
        def findUser(userId: UserId): QIO[Option[User]]
        def createUser(user: User): QIO[User]
        def createUsers(users: List[User]): QIO[List[User]]
        def updateUser(user: User): QIO[Unit]
        def deleteUser(user: User): QIO[Unit]
        def findByLastName(lastName: String): QIO[List[User]]
        def list(): QIO[List[User]]
        def userRoles(userId: UserId): QIO[List[Role]]
        def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
        def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
        def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
    }

    class ServiceImpl extends Service{

        lazy val userSchema = quote {querySchema[User](""""User"""")}

        lazy val roleSchema = quote {querySchema[Role](""""Role"""")}

        lazy val userToRoleSchema = quote {querySchema[UserToRole](""""UserToRole"""")}

        def findUser(userId: UserId): Result[Option[User]] = 
            dc.run(userSchema.filter(_.id == lift(userId.id))).map(_.headOption)
        
        def createUser(user: User): Result[User] = 
            dc.run(userSchema.insert(lift(user)).returning(u => u)) // почему не могу использовать identity?
        
        def createUsers(users: List[User]): Result[List[User]] = 
            dc.run(liftQuery(users).foreach(user => userSchema.insert(user).returning(u => u)))
            
        def updateUser(user: User): Result[Unit] = 
            dc.run(userSchema.filter(_.id == lift(user.id)).update(lift(user))).unit
        
        def deleteUser(user: User): Result[Unit] = 
            dc.run(userSchema.filter(_.id == lift(user.id)).delete).unit
        
        def findByLastName(lastName: String): Result[List[User]] = 
            dc.run(userSchema.filter(_.lastName == lift(lastName)))
        
        def list(): Result[List[User]] = 
            dc.run(userSchema)
        
        def userRoles(userId: UserId): Result[List[Role]] = 
            dc.run(
                for {
                    role <- userToRoleSchema.filter(_.userId == lift(userId.id))
                    res <- roleSchema.join(_.code == role.roleId)
                } yield res
            )
        
        def insertRoleToUser(roleCode: RoleCode, userId: UserId): Result[Unit] = 
            dc.run(userToRoleSchema.insert(_.roleId -> lift(roleCode.code), _.userId -> lift(userId.id))).unit
        
        def listUsersWithRole(roleCode: RoleCode): Result[List[User]] = 
            dc.run(
                for {
                    role <- roleSchema.filter(_.code == lift(roleCode.code))
                    userRole <- userToRoleSchema.join(_.roleId == role.code)
                    res <- userSchema.join(_.id == userRole.userId)
                } yield res
            )
        
        def findRoleByCode(roleCode: RoleCode): Result[Option[Role]] = 
            dc.run(
                for {
                    userRole <- userToRoleSchema.filter(_.roleId == lift(roleCode.code))
                    res <- roleSchema.join(_.code == userRole.roleId)
                } yield res
            ).map(_.headOption)
                
    }

    val live: ULayer[UserRepository] = ZLayer.succeed(new ServiceImpl)
}