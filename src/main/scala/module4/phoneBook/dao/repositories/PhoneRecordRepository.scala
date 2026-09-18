package module4.phoneBook.dao.repositories

import module4.phoneBook.db
import module4.phoneBook.dao.entities._
import zio._
import io.getquill.Ord

object PhoneRecordRepository {

  /**
   * В ZIO 2 + Quill 4.x:
   * - Используется PostgresZioJdbcContext
   */

  import db.Ctx._

  type PhoneRecordRepository = Service

  trait Service {
      def find(phone: String): ZIO[db.DataSource, Throwable, Option[PhoneRecord]]
      def list(): ZIO[db.DataSource, Throwable, List[PhoneRecord]]
      def insert(phoneRecord: PhoneRecord): ZIO[db.DataSource, Throwable, Unit]
      def update(phoneRecord: PhoneRecord): ZIO[db.DataSource, Throwable, Unit]
      def delete(id: String): ZIO[db.DataSource, Throwable, Unit]
  }

  class Impl extends Service {
     
     val phoneRecordSchema = quote {
       querySchema[PhoneRecord](""""PhoneRecord"""")
     }

     val addressSchema = quote {
       querySchema[Address](""""Address"""")
     }

    def find(phone: String): ZIO[db.DataSource, Throwable, Option[PhoneRecord]] = 
      run(phoneRecordSchema.filter(_.phone == lift(phone)))
      .map(_.headOption)
    
    def list(): ZIO[db.DataSource, Throwable, List[PhoneRecord]] = 
      run(phoneRecordSchema)
    
    def insert(phoneRecord: PhoneRecord): ZIO[db.DataSource, Throwable, Unit] = 
      run(phoneRecordSchema.insertValue(lift(phoneRecord))).unit
    
    def update(phoneRecord: PhoneRecord): ZIO[db.DataSource, Throwable, Unit] = 
      run(phoneRecordSchema.filter(_.id == lift(phoneRecord.id))
      .updateValue(lift(phoneRecord))).unit
    
    def delete(id: String): ZIO[db.DataSource, Throwable, Unit] = 
      run(phoneRecordSchema.filter(_.id == lift(id))
      .delete).unit

      def listWithAddress(): ZIO[db.DataSource, Throwable, List[(PhoneRecord, Address)]] = run(
        for {
           phoneRecord <- phoneRecordSchema
           address <- addressSchema if phoneRecord.addressId == address.id
        } yield (phoneRecord, address)
      )

      def listWithAddress2(): ZIO[db.DataSource, Throwable, List[(PhoneRecord, Address)]] = run(
        phoneRecordSchema
        .join(addressSchema)
        .on(_.addressId == _.id)
        .filter(v => v._1.phone == lift(""))
      )

      def listWithAddress3(): ZIO[db.DataSource, Throwable, List[(PhoneRecord, Address)]] = run(
        for {   
          phoneRecord <- phoneRecordSchema
          address <- addressSchema.join(_.id == phoneRecord.addressId)
        } yield (phoneRecord, address)
      )

      private val q = quote(phoneRecordSchema.filter(_.phone == lift("1234")))

      def count: ZIO[db.DataSource, Throwable, Long] = run(q.size)

      def paged: ZIO[db.DataSource, Throwable, List[PhoneRecord]] = 
        run(q.take(5).drop(20).sortBy(_.fio)(Ord.asc))
    
  }

 

  val live: ULayer[PhoneRecordRepository] = ZLayer.succeed(new Impl)
}
