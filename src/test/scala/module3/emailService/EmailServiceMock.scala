package module3.emailService

import zio._

object EmailServiceMock {

  /**
   * В ZIO 2:
   * - mock.Mock больше не доступен в том виде
   * - Моки создаются вручную через ZLayer
   */

  def make(onSendMail: Email => Unit = _ => ()): ULayer[EmailService] = 
    ZLayer.succeed(new EmailService.Service {
      def sendMail(email: Email): UIO[Unit] = ZIO.succeed(onSendMail(email))
    })

}
