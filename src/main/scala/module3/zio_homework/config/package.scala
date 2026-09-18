package module3.zio_homework

import zio._
import zio.config._
import zio.config.magnolia._
import zio.config.typesafe._

package object config {

  /**
   * Конфигурация для домашнего задания
   * 
   * В ZIO Config 4.x:
   * - Используется DeriveConfig вместо descriptor
   * - ConfigProvider заменяет ConfigSource
   */

  case class AppConfig(
    name: String,
    port: Int,
    debug: Boolean
  )

  object AppConfig {
    implicit val config: Config[AppConfig] = deriveConfig[AppConfig]

    val live: ZLayer[Any, Config.Error, AppConfig] = ZLayer {
      ZIO.config[AppConfig](config)
    }
  }

}
