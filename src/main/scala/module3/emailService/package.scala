package module3

import zio._


package object emailService {

    case class EmailAddress(value: String)
    case class Html(content: String)
    case class Email(to: EmailAddress, body: Html)

    /**
     * В ZIO 2:
     * - Has[A] больше не используется
     * - Сервисы объявляются как trait и предоставляются через ZLayer
     */

    type EmailService = EmailService.Service

    object EmailService {
        trait Service {
            def sendMail(email: Email): UIO[Unit]
        }

        val live: ULayer[EmailService] = ZLayer.succeed(new Service {
            def sendMail(email: Email): UIO[Unit] = 
                Console.printLine(email.toString()).orDie
        })

        def sendMail(email: Email): URIO[EmailService, Unit] = 
          ZIO.serviceWithZIO[EmailService](_.sendMail(email))
    }

}
