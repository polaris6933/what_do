package Client

import scala.util.control.Breaks
import java.net.InetSocketAddress

import akka.actor.{ Actor, Props, ActorSystem }
import akka.io.{ IO, Tcp }
import akka.util.ByteString


final case class SendMessage(message: ByteString)

object Client {
  def props(host: String, port: Int) = {
    Props(classOf[Client], new InetSocketAddress(host, port))
  }
}

class Client(remote: InetSocketAddress) extends Actor {
  import akka.io.Tcp._
  import context.system

  println("connecting to " +  remote.toString)

  val manager = IO(Tcp)
  manager ! Connect(remote)

  override def receive: Receive = {
    case CommandFailed(con: Connect) =>
      println("connection failed")
      println(con.failureMessage.toString)
      context stop self

    case c @ Connected(remote, local) =>
      println("successfully connected to " + remote)
      val connection = sender
      connection ! Register(self)

      context become {
        case SendMessage(message) =>
          connection ! Write(message)
        case data: ByteString =>
          println("Sending request data: " + data.utf8String)
          connection ! Write(data)
        case CommandFailed(w: Write) =>
          println("Failed to write request.")
          println(w.failureMessage.toString)
        case Received(data) =>
          println(data.utf8String)
        case "close" =>
          println("closing connection")
          connection ! Close
        case _: ConnectionClosed =>
          println("connection to server lost")
          context stop self
       }

    case _ =>
      println("something else is happening")

  }
}

object MainLoop extends App {
  val system = ActorSystem("application")
  val client = system.actorOf(Client.props("127.0.0.1", 8088))
  Thread.sleep(1000)

  client ! SendMessage(ByteString("register - dark - 1 - 50"))
  Thread.sleep(500)
  client ! SendMessage(ByteString("register - dark souls - 1 - 120"))
  Thread.sleep(500)
  client ! SendMessage(ByteString("register - dororo - 1 - 40"))
  Thread.sleep(500)
  client ! SendMessage(ByteString("register - mushishi - 2 - 20"))
  Thread.sleep(500)
  client ! SendMessage(ByteString("register - parasite - 1 - 10"))
  Thread.sleep(500)
  client ! SendMessage(ByteString("register - lotr - 0 - 10"))
  Thread.sleep(500)

  val loop = new Breaks
  loop.breakable {
    while (true) {
      val request = scala.io.StdIn.readLine("> ")
      if (request == "exit") {
        system.terminate()
        loop.break
      }
      client ! SendMessage(ByteString(request))
    }
  }
  println("exiting...")
}
