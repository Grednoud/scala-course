package module4.phoneBook

import zio._
import zio.config._
import zio.config.magnolia._
import zio.config.typesafe._

package object configuration {

  /**
   * В ZIO Config 4.x:
   * - Has[A] больше не используется
   * - descriptor заменен на deriveConfig
   * - TypesafeConfig.fromDefaultLoader -> ConfigProvider
   */

  case class Config(api: Api, liquibase: LiquibaseConfig, db2: DbConfig)
  
  case class LiquibaseConfig(changeLog: String)
  case class Api(host: String, port: Int)
  case class DbConfig(driver: String, url: String, user: String, password: String)
  
  object Config {
    implicit val config: zio.Config[Config] = deriveConfig[Config]
  }

  type Configuration = Config
  
  object Configuration {
    val live: ZLayer[Any, zio.Config.Error, Config] = ZLayer {
      ZIO.config[Config](Config.config)
    }
  }
}
