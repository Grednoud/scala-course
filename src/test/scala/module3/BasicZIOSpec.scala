package module3

import zio._
import zio.test._
import zio.test.Assertion._

import java.io.IOException


object BasicZIOSpec extends ZIOSpecDefault {

  /**
   * В ZIO 2 Test:
   * - DefaultRunnableSpec заменен на ZIOSpecDefault
   * - testM заменен на test
   * - environment.TestConsole заменен на TestConsole
   * - assert(x)(equalTo(y)) без изменений
   */

  val greeter: ZIO[Any, IOException, Unit] = for {
    _ <- Console.printLine("Как тебя зовут")
    name <- Console.readLine
    _ <- Console.printLine(s"Привет, $name")
  } yield ()


  val intGen: Gen[Any, Int] = Gen.int

  override def spec = suite("Basic")(
    suite("Arithmetic")(
      test("2 * 2 = 4")(
        assertTrue(2 * 2 == 4)
      ),
      test("division by zero") {
        assertTrue(
          try {
            2 / 0
            false
          } catch {
            case _: ArithmeticException => true
          }
        )
      }
    ),
    suite("Property based testing")(
      test("int addition is associative") {
        check(intGen, intGen, intGen) { (x, y, z) =>
          val left = (x + y) + z
          val right = x + (y + z)
          assertTrue(left == right)
        }
      }
    ),
    suite("Effect testing")(
      test("simple effect")(
        assertZIO(ZIO.succeed(2 * 2))(equalTo(4))
      )
    ),
    test("test console")(
      for {
        _ <- TestConsole.feedLines("Alex")
        _ <- greeter
        value <- TestConsole.output
      } yield {
          assertTrue(value.size == 2) && 
          assertTrue(value(1) == "Привет, Alex\n")
      }
    )
  )

}
