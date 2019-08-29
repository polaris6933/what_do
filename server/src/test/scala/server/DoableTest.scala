package doable

import org.scalatest.{FlatSpec, Matchers}

import Doable._
import Picker._

class DoableTest extends FlatSpec with Matchers {
  val d = new Doable("test", 1, 10, 0)

  "toString" should "print all the information of a Doable" in {
    d.toString shouldBe "test - 1 - 10 - 0"
  }

  "printable" should "present the information user-friendly" in {
    d.printable shouldBe d.time + " minutes on " + d.name
  }

  "addDust" should "increase the dust by one" in {
    val d1 = d.addDust
    d1.dust shouldBe d.dust + 1
  }

  "concatOptions" should "get the info for all given entries" in {
    val d1 = new Doable("dark", 1, 50, 0)
    val d2 = new Doable("parasite", 2, 135, 0)
    val d3 = new Doable("dark souls", 2, 60, 3)
    concatOptions(List(d1, d2, d3)) shouldBe
      "you can spend " + d1.time + " minutes on " + d1.name + " and\n" +
       d2.time + " minutes on " + d2.name + " and\n" +
       d3.time + " minutes on " + d3.name
  }

  it should "return nothing for an empty list" in {
    concatOptions(List[Doable]()) shouldBe "you can spend no time"
  }
}

class PickerTest extends FlatSpec with Matchers {
  val d1 = new Doable("dark", 1, 50)
  val d2 = new Doable("dark souls", 1, 120)
  val d3 = new Doable("dororo", 1, 40)
  val d4 = new Doable("mushishi", 2, 20)
  val d5 = new Doable("parasite", 1, 10)
  val d6 = new Doable("lotr", 0, 10)

  "choose" should "order properly by priority" in {
    val res = pick(List(d1, d2, d3, d4, d5, d6), 50, prioritySort)
    concatOptions(res) shouldBe concatOptions(List(d6, d3))
  }

  "choose" should "order properly by time" in {
    val res = pick(List(d1, d2, d3, d4, d5, d6), 50, timeSort)
    concatOptions(res) shouldBe concatOptions(List(d6, d5, d4))
  }

  "choose" should "order properly by dust" in {
    val d7 = d3.addDust.addDust
    val d8 = d4.addDust
    val d9 = d5.addDust
    val res = pick(List(d1, d2, d6, d7, d8, d9), 60, dustSort)
    concatOptions(res) shouldBe concatOptions(List(d7, d4))
  }
}
