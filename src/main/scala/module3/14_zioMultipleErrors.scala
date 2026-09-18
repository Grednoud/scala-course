package module3

import zio._

object zioMultipleErrors {

  sealed trait AppError
  case class DatabaseError(message: String) extends AppError
  case class ValidationError(message: String) extends AppError
  case class NetworkError(message: String) extends AppError

  // Работа с несколькими типами ошибок

  val dbOperation: IO[DatabaseError, String] = 
    ZIO.fail(DatabaseError("Connection failed"))

  val validationOperation: IO[ValidationError, Int] = 
    ZIO.fail(ValidationError("Invalid input"))

  // Объединение ошибок через общий тип
  val combined: IO[AppError, (String, Int)] = for {
    str <- dbOperation
    num <- validationOperation
  } yield (str, num)

  // Преобразование ошибки в общий тип
  val unified: IO[AppError, String] = 
    dbOperation.mapError(identity)

}
