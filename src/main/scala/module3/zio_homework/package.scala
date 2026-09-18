package module3

import zio._

import java.io.IOException

package object zio_homework {

  /**
   * Домашнее задание по ZIO
   * 
   * В ZIO 2:
   * - Стандартные сервисы (Console, Clock, Random) доступны без Has[]
   * - ZIO.effect заменен на ZIO.attempt
   * - ZIO.effectTotal заменен на ZIO.succeed (для чистых значений)
   */

  // Упражнение 1: Написать эффект, который читает строку с консоли
  lazy val readLine: ZIO[Any, IOException, String] = Console.readLine

  // Упражнение 2: Написать эффект, который печатает строку в консоль
  def printLine(line: String): ZIO[Any, IOException, Unit] = Console.printLine(line)

  // Упражнение 3: Написать echo программу
  lazy val echo: ZIO[Any, IOException, Unit] = for {
    line <- readLine
    _ <- printLine(line)
  } yield ()

  // Упражнение 4: Написать программу-калькулятор
  lazy val calculator: ZIO[Any, Throwable, Int] = for {
    _ <- printLine("Enter first number:")
    a <- readLine.flatMap(s => ZIO.attempt(s.toInt))
    _ <- printLine("Enter second number:")
    b <- readLine.flatMap(s => ZIO.attempt(s.toInt))
    sum = a + b
    _ <- printLine(s"Sum: $sum")
  } yield sum

}
