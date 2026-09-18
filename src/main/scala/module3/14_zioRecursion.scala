package module3

import zio._

object zioRecursion {

  /**
   * ZIO поддерживает безопасную рекурсию благодаря трамплинингу
   */

  // Пример: рекурсивный подсчет
  def countDown(n: Int): UIO[Unit] =
    if (n <= 0) ZIO.unit
    else Console.printLine(n.toString).orDie *> countDown(n - 1)

  // Пример: рекурсивный факториал
  def factorial(n: BigInt): UIO[BigInt] =
    if (n <= 1) ZIO.succeed(BigInt(1))
    else factorial(n - 1).map(_ * n)

  // Пример: бесконечный цикл (безопасный благодаря ZIO)
  def infiniteLoop: UIO[Nothing] =
    Console.printLine("Iteration").orDie *> ZIO.yieldNow *> infiniteLoop

  // Пример: цикл с условием выхода
  def loopUntil(condition: => Boolean): UIO[Unit] =
    if (condition) ZIO.unit
    else ZIO.yieldNow *> loopUntil(condition)

}
