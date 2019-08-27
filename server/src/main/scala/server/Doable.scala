package doable

import scala.collection.mutable.ListBuffer


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
    return name + " - " + time + " - " + priority + " - " + dust
  }
}

object Doable {
  def concatOptions(list: List[Doable]): String = {
    "you can spend " + list.map(_.printable).mkString(" and \n")
  }
}

object Picker {
  def prioritySort(d1: Doable, d2: Doable): Boolean = {
    if (d1.priority == d2.priority) {
      d1.time > d2.time
    } else {
      d1.priority < d2.priority
    }
  }

  def timeSort(d1: Doable, d2: Doable): Boolean = {
    if (d1.time == d2.time) {
      d1.priority > d2.priority
    } else {
      d1.time < d2.time
    }
  }

  def dustSort(d1: Doable, d2: Doable): Boolean = {
    if (d1.dust == d2.dust) {
      d1.time > d2.time
    } else {
      d1.dust < d2.dust
    }
  }

  private def pick(doables: List[Doable], timeAvailable: Int,
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
