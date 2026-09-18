package module4

import zio._
import zio.test._
import zio.test.Assertion._
import module4.homework.services.UserService
import module4.homework.dao.repository.UserRepository
import module4.homework.dao.entity.{User, UserId}
import java.util.UUID


object UserServiceSpec extends ZIOSpecDefault {

    /**
     * В ZIO 2 Test:
     * - DefaultRunnableSpec -> ZIOSpecDefault
     * - testM -> test
     * 
     * ВАЖНО: Эти тесты требуют Docker для запуска testcontainers.
     */

    import MigrationAspects._

    type Env = TestContainer.Postgres with 
        DBTransactor.DataSource with
        UserRepository.UserRepository with 
        UserService.UserService with
        LiquibaseService.Liqui with 
        LiquibaseService.LiquibaseService
    
    val layer: ZLayer[Any, Throwable, Env] = 
        TestContainer.postgres() >+> 
        DBTransactor.test >+> 
        LiquibaseService.liquibaseLayer ++ 
        UserRepository.live >+> 
        UserService.live ++ 
        LiquibaseService.live


    def spec = suite("UserServiceSpec")(
        test("getUser returns None for non-existent user")(
            for {
                result <- UserService.getUser(UserId(UUID.randomUUID().toString()))
            } yield assertTrue(result.isEmpty)
        ) @@ migrate(),
        test("createUser and then getUser returns the created user")(
            for {
                u <- ZIO.succeed(User(UUID.randomUUID().toString(), "John", "Doe", 30))
                created <- UserService.createUser(u)
                result <- UserService.getUser(created.typedId)
            } yield assertTrue(
                result.isDefined,
                result.get.firstName == "John",
                result.get.lastName == "Doe"
            )
        ) @@ migrate(),
        test("listUsers returns all created users")(
            for {
                u1 <- ZIO.succeed(User(UUID.randomUUID().toString(), "Alice", "Smith", 25))
                u2 <- ZIO.succeed(User(UUID.randomUUID().toString(), "Bob", "Jones", 35))
                _ <- UserService.createUser(u1)
                _ <- UserService.createUser(u2)
                result <- UserService.listUsers
            } yield assertTrue(result.length >= 2)
        ) @@ migrate()
    ).provideLayer(layer.orDie) @@ TestAspect.ifEnv("DOCKER_AVAILABLE")(_.toLowerCase == "true") @@ TestAspect.sequential
}
