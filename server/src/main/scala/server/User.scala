package user

import akka.util.ByteString
import scala.reflect.runtime.universe._

import doable.{ Doable, Picker }

class User(val doables: List[Doable], val pickStrat: (Doable, Doable) => Boolean) {
  def this() = {
    this(List[Doable](), Picker.prioritySort)
  }

  def register(name: String, priority: Int, time: Int): Option[User]= {
    if (doables.exists(x => x.name == name)) {
      return None
    }
    val newData = doables :+ new Doable(name, priority, time)
    Some(new User(newData, pickStrat))
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
    }
    new User(doables, sort)
  }

  def customStrat(first: String, second: String, third: String): User = {
    val sort = Picker.getCustomSort(first, second, third)
    new User(doables, sort)
  }

  def show(time: Int): List[Doable] = {
    Picker.pick(doables, time, pickStrat)
  }

  def pick(name: String): Option[User] = {
    if (!doables.exists(x => x.name == name)) {
      return None
    }
    val newData = for (entry <- doables if entry.name != name)
      yield entry.addDust
    Some(new User(newData, pickStrat))
  }

  def catalog(): ByteString = {
    ByteString(doables.map(_.toString).mkString("\n"))
  }

  def prioritize(name: String, priority: Int): Option[User] = {
    val entry = doables.find(_.name == name)
    if (entry.isEmpty) {
      return None
    }
    val newData = doables.filter(_.name != name) :+
                  new Doable(name, priority, entry.get.time)
    Some(new User(newData, pickStrat))
  }
}
