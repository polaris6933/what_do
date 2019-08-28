package user

import akka.util.ByteString
import scala.reflect.runtime.universe._

import doable.{ Doable, Picker }

// TODO: look into Try
// class EntryException(message: String) extends Exception(message) {
//   def this(message: String) = {
//     this(message)
//   }

//   def this(cause: Throwable) {
//     this(Option(cause).map(_.toString).orNull, cause)
//   }

//   def this() = {
//     this(null: String)
//   }
// }

class User(val doables: List[Doable], val pickStrat: (Doable, Doable) => Boolean) {
  // val validStrats = List("priority", "time", "dust")

  def this() = {
    this(List[Doable](), Picker.prioritySort)
  }

  def register(name: String, priority: Int, time: Int): User = {
    if (doables.exists(x => x.name == name)) {
      // throw new EntryException(name + " is already registered")
    }
    val newData = doables :+ new Doable(name, priority, time)
    new User(newData, pickStrat)
  }

  def changeStrat(strat: String): User = {
    // val mirror = runtimeMirror(getClass.getClassLoader)
    // val obj = mirror.reflect(Picker)
    // val method = typeOf[Picker.type].declaration((strat + "Sort"): TermName).asMethod
    // val sort = obj.reflectMethod(method)
    // new User(doables, sort)
    val sort: (Doable, Doable) => Boolean = strat match {
      case "priority" => Picker.prioritySort
      case "time" => Picker.timeSort
      case "dust" => Picker.dustSort
      // TODO: error handling
    }
    new User(doables, sort)
  }

  def customStrat(first: String, second: String, third: String): User = {
    val sort = Picker.getCustomSort(first, second, third)
    new User(doables, sort)
  }

  def show(time: Int): List[Doable] = {
    Picker.choose(doables, time, pickStrat)
  }

  def pick(name: String): User = {
    if (!doables.exists(x => x.name == name)) {
      // throw new EntryException(name + " is not registered")
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
      // throw new EntryException(name + " is not registered")
    }
    val newData = doables.filter(_.name != name) :+
                  new Doable(name, priority, entry.get.time)
    new User(newData, pickStrat)
  }
}
