package module3

import zio._

object zioErrorModel {

  type UserID
  type User

  trait GetUser {
    def getUserById(id: UserID): Task[User]
  }

  /**
   * В ZIO 2 обработка ошибок работает аналогично ZIO 1,
   * но с некоторыми улучшениями API
   */

  // Примеры работы с ошибками

  // Создание эффекта с ошибкой
  val failedEffect: IO[String, Nothing] = ZIO.fail("Something went wrong")

  // Восстановление из ошибки
  val recovered: UIO[String] = failedEffect.catchAll(err => ZIO.succeed(s"Recovered from: $err"))

  // Преобразование ошибки
  val mappedError: IO[Int, Nothing] = failedEffect.mapError(_.length)

  // Fold - обработка и успеха и ошибки
  val folded: UIO[String] = failedEffect.fold(
    err => s"Error: $err",
    success => s"Success: $success"
  )

  // FoldZIO - то же самое, но с эффектами
  val foldedZIO: UIO[String] = failedEffect.foldZIO(
    err => ZIO.succeed(s"Error: $err"),
    success => ZIO.succeed(s"Success: $success")
  )

  // Either - преобразование ошибки в Either
  val asEither: UIO[Either[String, Nothing]] = failedEffect.either

  // Cause - получение полной информации об ошибке
  val withCause: UIO[Exit[String, Nothing]] = failedEffect.exit

}
