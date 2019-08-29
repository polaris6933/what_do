package doable

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._


class Doable(val name: String, val priority: Int, val time: Int, val dust: Int) {
  def this(args: (String, Int, Int)) = {
    this(args._1, args._2, args._3, 0)
  }

  def addDust: Doable = {
    new Doable(this.name, this.priority, this.time, this.dust + 1)
  }

  def printable: String = {
    return time + " minutes on " + name
  }

  override def toString: String = {
    return name + " - " + priority + " - " + time + " - " + dust
  }
}

object Doable {
  def concatOptions(list: List[Doable]): String = {
    val prefix = "you can spend "
    val result = prefix + list.map(_.printable).mkString(" and\n")
    if (result == prefix) {
      result + "no time"
    } else {
      result
    }
  }
}

object Picker {
  def prioritySort(lhs: Doable, rhs: Doable): Boolean = {
    if (lhs.priority == rhs.priority) {
      lhs.time > rhs.time
    } else {
      lhs.priority < rhs.priority
    }
  }

  def timeSort(lhs: Doable, rhs: Doable): Boolean = {
    if (lhs.time == rhs.time) {
      lhs.priority < rhs.priority
    } else {
      lhs.time < rhs.time
    }
  }

  def dustSort(lhs: Doable, rhs: Doable): Boolean = {
    if (lhs.dust == rhs.dust) {
      lhs.time > rhs.time
    } else {
      lhs.dust > rhs.dust
    }
  }

  def getCustomSort(first: String, second: String, third: String) =
    (lhs: Doable, rhs: Doable) => {
    val mirror = runtimeMirror(getClass.getClassLoader)
    val lhsMirror = mirror.reflect(lhs)
    val rhsMirror = mirror.reflect(rhs)

    val firstField = typeOf[Doable].declaration(first: TermName).asMethod
    val secondField = typeOf[Doable].declaration(second: TermName).asMethod
    val thirdField = typeOf[Doable].declaration(third: TermName).asMethod

    val getField =
      (mirror: InstanceMirror, field: MethodSymbol) =>
        mirror.reflectField(field).get.asInstanceOf[Int]

    val rhsFirstField = getField(rhsMirror, firstField)
    val rhsSecondField = getField(rhsMirror, secondField)
    val rhsThirdField = getField(rhsMirror, thirdField)

    val lhsFirstField = getField(lhsMirror, firstField)
    val lhsSecondField = getField(lhsMirror, secondField)
    val lhsThirdField = getField(lhsMirror, thirdField)

    if (lhsFirstField == rhsFirstField) {
      if (lhsSecondField == rhsSecondField) {
        lhsThirdField < rhsThirdField
      } else {
        lhsSecondField < rhsSecondField
      }
    } else {
      lhsFirstField < rhsFirstField
    }
  }

  // TODO: consolidate these 2 functions
  def pick(doables: List[Doable], timeAvailable: Int,
    sorter: (Doable, Doable) => Boolean): List[Doable] = {
    val sorted = doables.sortWith(sorter)
    var timeTaken = 0
    var result = new ListBuffer[Doable]()
    for (d <- sorted) {
      if (timeTaken + d.time <= timeAvailable) {
        result += d
        timeTaken += d.time
      }
    }
    result.toList
  }

  def choose(doables: List[Doable], timeAvailable: Int, sort: (Doable, Doable) => Boolean):
    List[Doable] = {
    val valid = doables.filter(_.time <= timeAvailable)
    pick(valid, timeAvailable, sort)
  }
}
