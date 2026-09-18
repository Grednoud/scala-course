package module4

import zio._
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._
import module4.homework.dao.repository.UserRepository
import module4.homework.dao.entity.{User, UserId}
import java.util.UUID


object UserRepositorySpec extends ZIOSpecDefault {

    /**
     * В ZIO 2 Test:
     * - DefaultRunnableSpec -> ZIOSpecDefault
     * - testM -> test
     * - Has[A] больше не используется
     * - provideCustomLayer -> provide или provideLayer
     * 
     * ВАЖНО: Эти тесты требуют Docker для запуска testcontainers.
     * При отсутствии Docker тесты будут пропущены.
     */

    import MigrationAspects._
    val dc = DBTransactor.Ctx
    import dc._

    type Env = TestContainer.Postgres with 
        DBTransactor.DataSource with
        UserRepository.UserRepository with 
        LiquibaseService.Liqui with 
        LiquibaseService.LiquibaseService
    
    val layer: ZLayer[Any, Throwable, Env] = 
        TestContainer.postgres() >+> 
        DBTransactor.test >+> 
        LiquibaseService.liquibaseLayer ++ 
        UserRepository.live ++ 
        LiquibaseService.live


    val genName: Gen[Any, String] = Gen.alphaNumericString
    val genAge: Gen[Any, Int] = Gen.int(18, 120)
    val genUuid: Gen[Any, UUID] = Gen.uuid
    
    val genUser: Gen[Any, User] = for {
        uuid <- genUuid
        firstName <- genName
        lastName <- genName
        age <- genAge
    } yield User(uuid.toString(), firstName, lastName, age)


    val users: List[User] = List(
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(100) + 18),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(100) + 18),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(100) + 18),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(100) + 18),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(100) + 18),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(100) + 18),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(100) + 18),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(100) + 18),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(100) + 18),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(100) + 18)
    )
    val usersGen: Gen[Any, User] = Gen.fromIterable(users)



    def spec = suite("UserRepositorySpec")(
            test("метод list возвращает пустую коллекцию, на пустой базе")(
                for {
                    userRepo <- ZIO.service[UserRepository.UserRepository]
                    result <- userRepo.list()
                } yield assertTrue(result.isEmpty)
            ) @@ migrate(),
            test("методы create а затем findBy по созданному пользователю")(
                check(usersGen) { user => 
                    for {
                        userRepo <- ZIO.service[UserRepository.UserRepository]
                        created <- userRepo.createUser(user)
                        result <- userRepo.findUser(created.typedId)
                    } yield assertTrue(
                        result.isDefined,
                        result.get.id == created.id,
                        result.get.firstName == user.firstName
                    )
                }
            ) @@ migrate(),
            test("метод findBy по случайному id")(
                check(usersGen, genUuid) { (user, id) => 
                    for {
                        userRepo <- ZIO.service[UserRepository.UserRepository]
                        _ <- userRepo.createUser(user)
                        result <- userRepo.findUser(UserId(id.toString()))
                    } yield assertTrue(result.isEmpty) 
                }
            ) @@ migrate(),
            test("метод update должен обновлять только целевого пользователя")(
                for {
                    userRepo <- ZIO.service[UserRepository.UserRepository]
                    createdUsers <- userRepo.createUsers(users)
                    user = createdUsers.head
                    newFirstName = "Petr"
                    _ <- userRepo.updateUser(user.copy(firstName = newFirstName))
                    updated <- userRepo.findUser(user.typedId)
                    all <- userRepo.list()
                } yield assertTrue(
                    updated.isDefined,
                    updated.get.firstName == newFirstName,
                    all.filter(_.id != user.id).toSet == createdUsers.filter(_.id != user.id).toSet
                )
            ) @@ migrate(),
            test("метод delete должен удалять только целевого пользователя")(
                for {
                    userRepo <- ZIO.service[UserRepository.UserRepository]
                    createdUsers <- userRepo.createUsers(users)
                    user = createdUsers.last
                    _ <- userRepo.deleteUser(user)
                    all <- userRepo.list()
                } yield assertTrue(
                    all.length == 9,
                    all.toSet == createdUsers.filter(_.id != user.id).toSet
                )
            ) @@ migrate(),
            test("метод findByLastName должен находить пользователя")(
                for {
                    userRepo <- ZIO.service[UserRepository.UserRepository]
                    createdUsers <- userRepo.createUsers(users)
                    user = createdUsers(5)
                    result <- userRepo.findByLastName(user.lastName)
                } yield assertTrue(
                    result.length == 1,
                    result.head.lastName == user.lastName
                )
            ) @@ migrate(),

        ).provideLayer(layer.orDie) @@ TestAspect.ifEnv("DOCKER_AVAILABLE")(_.toLowerCase == "true") @@ sequential
}
