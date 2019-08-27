package server

import java.net.InetSocketAddress
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Success, Failure}
import scala.util.control.Breaks

import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.io.{ IO, Tcp }
import Tcp._
import akka.pattern.ask
import akka.util.{ ByteString, Timeout }

import store.Store


class Server extends Actor {
  import context.system

  IO(Tcp) ! Bind(self, new InetSocketAddress("localhost", 8088))

  def receive = {
    case b @ Bound(localAddress) =>
      println(b)
      context.parent ! b

    case CommandFailed(_: Bind) =>
      context.stop(self)

    case Connected(remote, local) =>
      val handler = context.actorOf(Props[ConnectionHandler])
      val connection = sender()
      println("connected " + remote + " " + local)
      connection ! Register(handler)
  }
}

class ConnectionHandler extends Actor {
  val store = context.actorOf(Props[Store], "store")

  def receive = {
    case Received(data) =>
      try {
        implicit val timeout = Timeout(5 seconds)
        val future = store ? Store.ClientMessage(data.utf8String)
        val result = Await.result(future, timeout.duration).asInstanceOf[Store.Response]
        sender ! Write(result.message)
      } catch {
        case e: java.util.concurrent.TimeoutException =>
          sender ! Write(ByteString("the server took too long to process your request"))
      }
    case PeerClosed =>
      println("peer closed")
      context.stop(self)
    case ErrorClosed(msg) =>
      println(msg)
      context.stop(self)
  }
}

object MainLoop extends App {
  val system = ActorSystem("application")
  val server = system.actorOf(Props[Server], "server")

  val loop = new Breaks
  loop.breakable {
    while (true) {
      val command = scala.io.StdIn.readLine()
      if (command == "exit") {
        system.terminate()
        loop.break
      }
    }
  }
  println("exiting...")
}

