package module3

import zio._

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.Try
import java.io.IOException


/** **
 * ZIO[-R, +E, +A] ----> R => Either[E, A]
 *
 */

object toyModel {


  /**
   * Используя executable encoding реализуем свой zio
   */
   
   case class ZIO[-R, +E, +A](run: R => Either[E, A]){ self =>

      def map[B](f: A => B): ZIO[R, E, B] = 
        ZIO(r => self.run(r).map(f))
      
      def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] = 
         ZIO( r => self.run(r).fold(ZIO.fail, f).run(r))
   }




  /**
   * Реализуем конструкторы под названием effect и fail
   */
  object ZIO{
    
    def effect[A](a: => A): ZIO[Any, Throwable, A] = try{
      ZIO(_ => Right(a))
    } catch{
      case e: Throwable => fail(e)
    }

    def fail[E](e: E): ZIO[Any, E, Nothing] =  
      ZIO(_ => Left(e))
  }







  /** *
   * Напишите консольное echo приложение с помощью нашего игрушечного ZIO
   */

   lazy val echo: ZIO[Any, Throwable, Unit] = for{
     str <- ZIO.effect(StdIn.readLine())
     _ <- ZIO.effect(println(str))
   } yield ()




  type Error
  type Environment

  // ZIO[-R, +E, +A]

  //type T[A] = ZIO[Any, Throwable, A]



  lazy val t1: zio.Task[Int] = ??? // ZIO[Any, Throwable, Int]
  lazy val io1: zio.IO[Error, Int] = ??? // ZIO[Any, Error, Int]
  lazy val rio1: zio.RIO[Environment, Int] = ??? // ZIO[Environment, Throwable, Int]
  lazy val urio1: zio.URIO[Environment, Int] = ??? // ZIO[Environment, Nothing, Int]
  lazy val uio1: zio.UIO[Int] = ??? // ZIO[Any, Nothing, Int]
}

object zioConstructors {


  // константа
  val z1: UIO[Int] = ZIO.succeed(7)


  // любой эффект
  val z2: Task[Unit] = ZIO.attempt(println("Hello"))

  // любой не падающий эффект

  val z3: UIO[Unit] = ZIO.succeed(println("Hello"))




  // From Future
  val f: Future[Int] = ???
  val z4: Task[Int] = ZIO.fromFuture(ec => f)


  // From try
  lazy val t: Try[String] = ???
  lazy val z5: Task[String] = ZIO.fromTry(t)



  // From either
  lazy val e: Either[String, Int] = ???
  lazy val z6: IO[String,Int] = ZIO.fromEither(e)



  // From option
  lazy val opt : Option[Int] = ???
  lazy val z7: IO[Option[Nothing], Int] = ZIO.fromOption(opt)
  val z77: URIO[Any,Option[Int]] = z7.option
  val z78: ZIO[Any,Option[Nothing],Int] = z77.some



  // From function
  lazy val z8: URIO[String, Int] = ZIO.serviceWith[String](str => str.toInt)

  // особые версии конструкторов

  lazy val unit1: UIO[Unit] = ZIO.unit

  lazy val none1: UIO[Option[Nothing]] = ZIO.none

  lazy val never1: UIO[Nothing] = ZIO.never // while(true)

  lazy val die1: ZIO[Any, Nothing, Nothing] = ZIO.die(new Throwable("Ooops"))

  lazy val fail1: ZIO[Any, Int, Nothing] = ZIO.fail(1)

}



object zioOperators {

  /** *
   *
   * 1. Создать ZIO эффект который будет читать строку из консоли
   */

  lazy val readLine = ???

  /** *
   *
   * 2. Создать ZIO эффект который будет писать строку в консоль
   */

  def writeLine(str: String) = ???

  /** *
   * 3. Создать ZIO эффект котрый будет трансформировать эффект содержащий строку в эффект содержащий Int
   */

  lazy val lineToInt = ???
  /** *
   * 3.Создать ZIO эффект, который будет работать как echo для консоли
   *
   */

  lazy val echo = ???

  /**
   * Создать ZIO эффект, который будет привествовать пользователя и говорить, что он работает как echo
   */

  lazy val greetAndEcho = ???

  // Другие варианты композиции

  lazy val a1: Task[Unit] = ??? // println()
  lazy val b1: Task[String] = ???


  lazy val z9: ZIO[Any,Throwable,(Unit, String)] = for {
    u <- a1
    s <- b1
  } yield (u, s)

  lazy val z10: ZIO[Any,Throwable, String] = a1 *> b1

  lazy val z11: ZIO[Any,Throwable, Unit] = a1 <* b1


  // greet and echo улучшенный
  lazy val greetAndEchoImproved: ZIO[Any, Throwable, Unit] = ???


  /**
   * Используя уже созданные эффекты, написать программу, которая будет считывать поочереди считывать две
   * строки из консоли, преобразовывать их в числа, а затем складывать их
   */

  val r1 = ???

  /**
   * Второй вариант
   */

  val r2: ZIO[Any, Throwable, Int] = ???

  /**
   * Доработать написанную программу, чтобы она еще печатала результат вычисления в консоль
   */

  lazy val r3 = ???


  lazy val a: Task[Int] = ???
  lazy val b: Task[String] = ???

  /**
   * последовательная комбинация эффектов a и b
   */
  lazy val ab1: ZIO[Any, Throwable, (Int, String)] = ???

  /**
   * последовательная комбинация эффектов a и b
   */
  lazy val ab2: ZIO[Any, Throwable, Int] = ??? 

  /**
   * последовательная комбинация эффектов a и b
   */
  lazy val ab3: ZIO[Any, Throwable, String] = ??? 


  /**
   * Последовательная комбинация эффета b и b, при этом результатом должна быть конкатенация
   * возвращаемых значений
   */
  lazy val ab4: ZIO[Any,Throwable, String] = b.zipWith(b)(_ + _)


  /**
    * 
    * Другой эффект в случае ошибки
    */

    val ab5 = ab2.orElse(ab3)

  /**
    * 
    * A as B
    */



  def readFile(fileName: String): ZIO[Any, IOException, String] = ???

  // из эффекта с ошибкой, в эффект который не падает

  val d: URIO[Any,String] = readFile("").orDie
  
}
