package module3

import scala.annotation.tailrec
import scala.io.StdIn


object functional_effects {

  /**
   * Демонстрация функционального эффекта без использования ZIO
   * Показывает, как можно построить простой интерпретатор для консольных операций
   */

  sealed trait Console[+A] { self =>
    def map[B](f: A => B): Console[B] =
      flatMap(a => Console.succeed(f(a)))

    def flatMap[B](f: A => Console[B]): Console[B] =
      Console.FlatMap(self, f)

    def *>[B](that: Console[B]): Console[B] =
      self.flatMap(_ => that)
  }

  object Console {

    def succeed[A](a: => A): Console[A] = Succeed(() => a)

    val readLine: Console[String] = ReadLine
    def printLine(str: String): Console[Unit] = PrintLine(str)

    case object ReadLine extends Console[String]
    final case class PrintLine(str: String) extends Console[Unit]
    final case class Succeed[A](value: () => A) extends Console[A]
    final case class FlatMap[A, B](console: Console[A], f: A => Console[B]) extends Console[B]

    @tailrec
    def run[A](console: Console[A]): A = console match {
      case ReadLine => StdIn.readLine()
      case PrintLine(str) => println(str)
      case Succeed(value) => value()
      case FlatMap(inner, f) =>
        inner match {
          case ReadLine => run(f(StdIn.readLine()))
          case PrintLine(str) =>
            println(str)
            run(f(()).asInstanceOf[Console[A]])
          case Succeed(value) => run(f(value()))
          case FlatMap(console2, f2) =>
            run(FlatMap(console2, (a: Any) => FlatMap(f2(a), f)))
        }
    }

    val greeter: Console[Unit] = for {
      _ <- Console.printLine("What is your name?")
      name <- Console.readLine
      _ <- Console.printLine(s"Hello, $name!")
    } yield ()
  }
}
