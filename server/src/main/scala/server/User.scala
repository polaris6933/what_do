package user

import akka.util.ByteString

import doable.{ Doable, Picker }

// TODO: user definied strategies

// TODO: look into Try
class EntryException(message: String) extends Exception(message) {
  def this(message: String) = {
    this(message)
  }

  def this(cause: Throwable) {
    this(Option(cause).map(_.toString).orNull, cause)
  }

  def this() = {
    this(null: String)
  }
}

class User(val doables: List[Doable], val pickStrat: String) {
  def this() = {
    this(List[Doable](), "priority")
  }

  def register(name: String, priority: Int, time: Int): User = {
    if (doables.exists(x => x.name == name)) {
      throw new EntryException(name + " is already registered")
    }
    val newData = doables :+ new Doable(name, priority, time)
    new User(newData, pickStrat)
  }

  def show(time: Int): List[Doable] = {
    val sort: (Doable, Doable) => Boolean = pickStrat match {
      case "priority" => Picker.prioritySort
      case "time" => Picker.timeSort
      case "dust" => Picker.dustSort
    }
    Picker.choose(doables, time, sort)
  }

  def pick(name: String): User = {
    if (!doables.exists(x => x.name == name)) {
      throw new EntryException(name + " is not registered")
    }
    val newData = for (entry <- doables if entry.name != name)
      yield entry.addDust
    new User(newData, pickStrat)
  }

  def catalog(): ByteString = {
    ByteString(doables.map(_.toString).mkString("\n"))
  }

  def prioritize(name: String, priority: Int): User = {
    val entry = doables.find(_.name == name)
    if (entry.isEmpty) {
      throw new EntryException(name + " is not registered")
    }
    val newData = doables.filter(_.name != name) :+
                  new Doable(name, priority, entry.get.time)
    new User(newData, pickStrat)
  }
}
