package module4.phoneBook.dao.repositories

import module4.phoneBook.db
import module4.phoneBook.dao.entities.Address
import zio._

object AddressRepository {

  /**
   * В ZIO 2 + Quill 4.x:
   * - Используется PostgresZioJdbcContext
   */

  import db.Ctx._

  type AddressRepository = Service

  trait Service {
      def findBy(id: String): ZIO[db.DataSource, Throwable, Option[Address]]
      def insert(address: Address): ZIO[db.DataSource, Throwable, Unit]
      def update(address: Address): ZIO[db.DataSource, Throwable, Unit]
      def delete(id: String): ZIO[db.DataSource, Throwable, Unit]
  }

  class ServiceImpl extends Service {

      val addressSchema = quote {
        querySchema[Address](""""Address"""")
      }

      def findBy(id: String): ZIO[db.DataSource, Throwable, Option[Address]] = 
         run(addressSchema.filter(_.id == lift(id))).map(_.headOption)
      
      def insert(address: Address): ZIO[db.DataSource, Throwable, Unit] = 
        run(addressSchema.insertValue(lift(address))).map(_ => ())
      
      def update(address: Address): ZIO[db.DataSource, Throwable, Unit] = 
        run(addressSchema.updateValue(lift(address))).map(_ => ())
      
      def delete(id: String): ZIO[db.DataSource, Throwable, Unit] = 
        run(addressSchema.filter(_.id == lift(id)).delete).map(_ => ())
      
  }

  val live: ULayer[AddressRepository] = ZLayer.succeed(new ServiceImpl)
}
