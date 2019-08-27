package store

import akka.actor.Actor
import akka.util.ByteString

import doable.{ Doable, Picker }
import user.{ User, EntryException }


object Store {
  final case class ClientMessage(message: String)
  final case class Response(message: ByteString)
}

class Store extends Actor {
  import Store._

  val userData = scala.collection.mutable.Map[String, User]()
  userData("joe") = new User

  override def receive = {
    case ClientMessage(message) =>
      val response = validateRequest(message.split("-"))()
      sender() ! Response(response)
  }

  private def validateRequest(messageRaw: Array[String]): (() => ByteString) = {
    val message = messageRaw.map(_.trim)
    val validLength: (Int) => Boolean = len => message.length  == len
    val error: (String) => () => ByteString = msg => () => {ByteString(msg)}

    message(0) match {
      case "register" =>
        if (!validLength(4)) {
          return error("invalid number of arguments")
        }
        try {
          val priority = message(2).toInt
          val time = message(3).toInt
          return () => register(message(1), priority, time)
        } catch {
          case e: NumberFormatException =>
            return error("invalid argument for time / priority")
        }

      case "show" =>
        if (!validLength(2)) {
          return error("invalid number of arguments")
        }
        try {
          val time = message(1).toInt
          () => show(time)
        } catch {
          case e: NumberFormatException =>
            return error("invalid argument for time")
        }

      case "pick" =>
        if (!validLength(2)) {
          return error("invalid number of arguments")
        }
        () => pick(message(1))

      case "catalog" =>
        if (!validLength(1)) {
          return error("invalid number of arguments")
        }
        () => catalog

      case "prioritize" =>
        if (!validLength(3)) {
          return error("invalid number of arguments")
        }
        try {
          val priority = message(2).toInt
          () => prioritize(message(1), priority)
        } catch {
          case e: NumberFormatException =>
            return error("invalid argument for time")
        }

      case _ =>
        error("unsupported method")
    }
  }

  private def register(name: String, priority: Int, time: Int): ByteString = {
    val user = userData("joe")
    try {
      userData("joe") = user.register(name, priority, time)
    } catch {
      case e: EntryException =>
        return ByteString(e.message)
    }
    ByteString("ok")
  }

  private def show(time: Int): ByteString = {
    val user = userData("joe")
    val options = user.show(time)
    ByteString(Doable.concatOptions(options))
  }

  private def pick(name: String): ByteString = {
    val user = userData("joe")
    try {
      userData("joe") = user.pick(name)
    } catch {
      case e: EntryException =>
        return ByteString(e.message)
    }
    ByteString("ok")
  }

  private def catalog: ByteString = {
    userData("joe").catalog
  }

  private def prioritize(name: String, priority: Int): ByteString = {
    val user = userData("joe")
    try {
      userData("joe") = user.prioritize(name, priority)
    } catch {
      case e: EntryException =>
        return ByteString(e.message)
    }
    ByteString("ok")
  }
}
