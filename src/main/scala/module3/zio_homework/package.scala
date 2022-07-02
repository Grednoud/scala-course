package module3

import zio.{Has, Task, ULayer, ZIO, ZLayer}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  def readInt = putStrLn("Введите число от 1 до 3") *> getStrLn.flatMap(str =>  ZIO.effect(str.toInt))

  def readIntOrRetry: ZIO[Console, Throwable, Int] = readInt.orElse(putStrLn("Введено не натуральное число") *> readIntOrRetry)

  def checkInput(rnd: Int): ZIO[Console, Throwable, Unit] = readIntOrRetry.flatMap{
    case guess if (1 > guess || guess > 3) => putStrLn("Введено некорректное число") *> checkInput(rnd)
    case guess if (guess != rnd) => putStrLn("Не угадали!") *> checkInput(rnd)
    case _ => putStrLn("Угадали!")
  }

  lazy val guessProgram = for {
    _ <- putStrLn("Угадайте число от 1 до 3")
    rnd <- nextIntBetween(1, 3)
    _ <- checkInput(rnd)
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */
  // не до конца понял что требуется в этом задании
  def doWhile[R,E](f: ZIO[R,E,Boolean]) = f.repeatUntil(_)

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault = config.load.orElse( 
      ZIO.succeed(config.AppConfig("default", "default"))
        .flatMap(cfg => putStrLn(cfg.toString()).map(_ => cfg))
  )


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff = nextIntBetween(0, 10).delay(1 second)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = ZIO.replicate(10)(eff)
  // lazy val effects = List.fill(10)(eff)

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = zioConcurrency.printEffectRunningTime(
    for {
      sum <- ZIO.collectAllPar(effects).map(_.sum)
      _ <- putStrLn(sum.toString())
    } yield sum
  )


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = app


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

   object TimeLogger {
      type TimeLogger = Has[TimeloggerImpl]

      class TimeloggerImpl {
        def printEffectRunningTime[R, E, A](f: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] = 
          for{
            start <- zio.clock.currentTime(TimeUnit.SECONDS)
            z <- f
            finish <- zio.clock.currentTime(TimeUnit.SECONDS)
            _ <- putStrLn(s"Running time: ${finish - start}")
          } yield z
      }

      val live = ZLayer.succeed(new TimeloggerImpl)

      def printEffectRunningTime[R, E, A](f: ZIO[R, E, A]) = ZIO.accessM[TimeLogger with Console with Clock with R](_.get.printEffectRunningTime(f))
   }


   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg = TimeLogger.printEffectRunningTime(
    for {
      sum <- ZIO.collectAllPar(effects).map(_.sum)
      _ <- putStrLn(sum.toString())
    } yield sum
  )

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = appWithTimeLogg.provideSomeLayer[Console with Random with Clock](TimeLogger.live)
  
}
