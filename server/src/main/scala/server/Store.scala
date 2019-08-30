package store

import akka.actor.Actor
import akka.util.ByteString

import doable.{ Doable, Picker }
import user.User


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

      case "strat" =>
        if (!validLength(2)) {
          return error("invalid number of arguments")
        }
        () => strat(message(1))

      case "custom_strat" =>
        if (!validLength(4)) {
          return error("invalid number of arguments")
        }
        () => customStrat(message(1), message(2), message(3))

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

      case "clear" =>
        if (!validLength(1)) {
          return error("invalid number of arguments")
        }
        () => clear

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
    val registered = user.register(name, priority, time)
    if (registered.isEmpty) {
      return ByteString(name + " is already registered")
    }
    userData("joe") = registered.get
    ByteString("ok")
  }

  private def show(time: Int): ByteString = {
    val user = userData("joe")
    val options = user.show(time)
    ByteString(Doable.concatOptions(options))
  }

  private def pick(name: String): ByteString = {
    val user = userData("joe")
    val picked = user.pick(name)
    if (picked.isEmpty) {
      return ByteString(name + " is not registered")
    }
    userData("joe") = picked.get
    ByteString("ok")
  }

  private def strat(strat: String): ByteString = {
    val user = userData("joe")
    userData("joe") = user.changeStrat(strat)
    ByteString("ok")
  }

  private def customStrat(first: String, second: String, third: String): ByteString = {
    val user = userData("joe")
    val validFields = List("priority", "time", "dust")
    val inputIsValid = for (field <- List(first, second, third))
      yield validFields.contains(field)
    if (inputIsValid.contains(false)) {
      return ByteString("invalid strategy ordering")
    }
    userData("joe") = user.customStrat(first, second, third)
    ByteString("ok")
  }

  private def catalog: ByteString = {
    userData("joe").catalog
  }

  private def clear: ByteString = {
    userData("joe") = new User
    ByteString("ok")
  }

  private def prioritize(name: String, priority: Int): ByteString = {
    val user = userData("joe")
    val prioritized = user.prioritize(name, priority)
    if (prioritized.isEmpty) {
      return ByteString(name + " is not registered")
    }
    userData("joe") = prioritized.get
    ByteString("ok")
  }
}
