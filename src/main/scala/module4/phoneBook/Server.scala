package module4.phoneBook

import cats.effect.{ExitCode => CatsExitCode}
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.ember.server.EmberServerBuilder
import zio._
import zio.interop.catz._
import module4.phoneBook.configuration.{Config => AppConfig, Configuration}
import api.PhoneBookAPI
import module4.phoneBook.services.PhoneBookService
import module4.phoneBook.db._
import module4.phoneBook.dao.repositories.{PhoneRecordRepository, AddressRepository}
import com.comcast.ip4s._


object Server {

    /**
     * В ZIO 2 + http4s 0.23+:
     * - Has[A] больше не используется
     * - BlazeServerBuilder заменен на EmberServerBuilder
     */

    type AppEnvironment = PhoneBookService.PhoneBookService with 
      PhoneRecordRepository.PhoneRecordRepository with 
      AddressRepository.AddressRepository with 
      Configuration with 
      LiquibaseService.Liqui with 
      DataSource

    val appEnvironment: ZLayer[Any, Throwable, AppEnvironment] = 
      Configuration.live >+> 
      zioDS >+> 
      LiquibaseService.liquibaseLayer ++ 
      PhoneRecordRepository.live >+> 
      AddressRepository.live >+> 
      PhoneBookService.live

    type AppTask[A] = RIO[AppEnvironment, A]

    val httpApp = Router[AppTask]("/phoneBook" -> new PhoneBookAPI[AppEnvironment]().route).orNotFound

    val server: ZIO[AppEnvironment, Throwable, Unit] = for {
      config <- ZIO.service[AppConfig]
      _ <- LiquibaseService.performMigration
      _ <- ZIO.executor.flatMap { executor =>
          EmberServerBuilder
            .default[AppTask]
            .withHost(Host.fromString(config.api.host).getOrElse(host"0.0.0.0"))
            .withPort(Port.fromInt(config.api.port).getOrElse(port"8080"))
            .withHttpApp(httpApp)
            .build
            .useForever
        }
    } yield ()
}
