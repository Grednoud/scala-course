import scala.util.control.Breaks._
import module3.functional_effects
import module3.zioRecursion
import zio._

import scala.language.postfixOps

object App {

  def main(args: Array[String]): Unit = {
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(ZIO.succeed(println("Hello from ZIO 2!"))).getOrThrowFiberFailure()
    }
  }

}

object ZioApp extends ZIOAppDefault {
  
  def run: ZIO[Any, Any, Any] = 
    Console.printLine("Hello from ZIO 2 App!")
}
