package module4.phoneBook.api

import zio._
import io.circe.Decoder
import io.circe.Encoder
import org.http4s.EntityEncoder
import org.http4s.EntityDecoder
import io.circe.generic.auto._
import org.http4s.circe._
import zio.interop.catz._
import org.http4s.dsl.Http4sDsl
import org.http4s.HttpRoutes
import module4.phoneBook.dto._
import module4.phoneBook.services.PhoneBookService
import module4.phoneBook.db


class PhoneBookAPI[R <: PhoneBookService.PhoneBookService with db.DataSource] {

    type PhoneBookTask[A] = RIO[R, A]

    val dsl = Http4sDsl[PhoneBookTask]
    import dsl._


    implicit def jsonDecoder[A](implicit decoder: Decoder[A]): EntityDecoder[PhoneBookTask, A] = 
      jsonOf[PhoneBookTask, A]
    implicit def jsonEncoder[A](implicit decoder: Encoder[A]): EntityEncoder[PhoneBookTask, A] = 
      jsonEncoderOf[PhoneBookTask, A]
  

    def route: HttpRoutes[PhoneBookTask] = HttpRoutes.of[PhoneBookTask] {
      case GET -> Root / phone => PhoneBookService.find(phone).foldZIO(
        _ => NotFound(),
        result => Ok(result)
      )
      case req @ POST -> Root => (for {
        record <- req.as[PhoneRecordDTO]
        result <- PhoneBookService.insert(record)
      } yield result).foldZIO(
        err => BadRequest(err.getMessage()),
        result => Ok(result)
      )
      case req @ PUT -> Root / id / addressId => (for {
        record <- req.as[PhoneRecordDTO]
        _ <- PhoneBookService.update(id, addressId, record)
      } yield ()).foldZIO(
        err => BadRequest(err.getMessage()),
        _ => Ok("Updated")
      )
      case DELETE -> Root / id => PhoneBookService.delete(id).foldZIO(
        _ => BadRequest("Not found"),
        _ => Ok("Deleted")
      )
    }
}
