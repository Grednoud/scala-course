package module3

import zio._

object zioOtherDS {

  /**
   * Другие структуры данных ZIO
   */

  // Ref - изменяемая ссылка
  val refExample: UIO[Int] = for {
    ref <- Ref.make(0)
    _ <- ref.update(_ + 1)
    _ <- ref.update(_ + 2)
    value <- ref.get
  } yield value

  // Promise - одноразовое значение
  val promiseExample: UIO[Int] = for {
    promise <- Promise.make[Nothing, Int]
    _ <- promise.succeed(42).fork
    value <- promise.await
  } yield value

  // Queue - очередь
  val queueExample: UIO[Int] = for {
    queue <- Queue.bounded[Int](10)
    _ <- queue.offer(1)
    _ <- queue.offer(2)
    first <- queue.take
    second <- queue.take
  } yield first + second

  // Hub - широковещательная очередь
  // Примечание: subscribe требует Scope, поэтому оборачиваем в scoped
  val hubExample: ZIO[Any, Nothing, Chunk[Int]] = ZIO.scoped {
    for {
      hub <- Hub.bounded[Int](10)
      subscription <- hub.subscribe
      _ <- hub.publish(1)
      _ <- hub.publish(2)
      values <- subscription.takeAll
    } yield values
  }

  // Semaphore - семафор
  val semaphoreExample: UIO[Unit] = for {
    semaphore <- Semaphore.make(1)
    _ <- semaphore.withPermit {
      Console.printLine("Critical section").orDie
    }
  } yield ()

}
