package module4.phoneBook.services

import zio._
import module4.phoneBook.dto._
import module4.phoneBook.dao.repositories.{PhoneRecordRepository, AddressRepository}
import module4.phoneBook.db
import module4.phoneBook.dao.entities.{PhoneRecord, Address}

object PhoneBookService {

  /**
   * В ZIO 2:
   * - Has[A] больше не используется
   */

   import db.Ctx._

   type PhoneBookService = Service
  
   trait Service {
     def find(phone: String): ZIO[db.DataSource, Option[Throwable], (String, PhoneRecordDTO)]
     def insert(phoneRecord: PhoneRecordDTO): RIO[db.DataSource, String]
     def update(id: String, addressId: String, phoneRecord: PhoneRecordDTO): RIO[db.DataSource, Unit]
     def delete(id: String): RIO[db.DataSource, Unit]
   }

    class Impl(
      phoneRecordRepository: PhoneRecordRepository.Service, 
      addressRepository: AddressRepository.Service
    ) extends Service {

        def find(phone: String): ZIO[db.DataSource, Option[Throwable], (String, PhoneRecordDTO)] = for {
          result <- phoneRecordRepository.find(phone).some
        } yield (result.id, PhoneRecordDTO.from(result))

        def insert(phoneRecord: PhoneRecordDTO): RIO[db.DataSource, String] = for {
          uuid <- Random.nextUUID.map(_.toString())
          uuid2 <- Random.nextUUID.map(_.toString())
          address = Address(uuid, phoneRecord.zipCode, phoneRecord.address)
          _ <- transaction(
                  for {
                    _ <- addressRepository.insert(address)
                    _ <- phoneRecordRepository.insert(PhoneRecord(uuid2, phoneRecord.phone, phoneRecord.fio, address.id))
                  } yield ()
                )
        } yield uuid
        
        def update(id: String, addressId: String, phoneRecord: PhoneRecordDTO): RIO[db.DataSource, Unit] = for {
            _ <- phoneRecordRepository.update(PhoneRecord(id, phoneRecord.phone, phoneRecord.fio, addressId))
        } yield ()
        
        def delete(id: String): RIO[db.DataSource, Unit] = for {
            _ <- phoneRecordRepository.delete(id)
        } yield ()
        
    }

    val live: ZLayer[PhoneRecordRepository.Service with AddressRepository.Service, Nothing, PhoneBookService] = 
      ZLayer.fromFunction((repo: PhoneRecordRepository.Service, addressRepo: AddressRepository.Service) => 
        new Impl(repo, addressRepo): PhoneBookService
      )

    def find(phone: String): ZIO[PhoneBookService with db.DataSource, Option[Throwable], (String, PhoneRecordDTO)] =
      ZIO.serviceWithZIO[PhoneBookService](_.find(phone))

    def insert(phoneRecord: PhoneRecordDTO): RIO[PhoneBookService with db.DataSource, String] =
      ZIO.serviceWithZIO[PhoneBookService](_.insert(phoneRecord))

    def update(id: String, addressId: String, phoneRecord: PhoneRecordDTO): RIO[PhoneBookService with db.DataSource, Unit] =
      ZIO.serviceWithZIO[PhoneBookService](_.update(id, addressId, phoneRecord))

    def delete(id: String): RIO[PhoneBookService with db.DataSource, Unit] =
      ZIO.serviceWithZIO[PhoneBookService](_.delete(id))

}
