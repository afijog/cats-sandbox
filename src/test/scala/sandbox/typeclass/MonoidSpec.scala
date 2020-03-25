package sandbox.typeclass

import cats.Monoid
import cats.instances.int._
import cats.instances.list._
import cats.instances.map._
import cats.instances.option._
import cats.instances.string._
import cats.instances.tuple._
import cats.syntax.monoid._
import cats.syntax.option._
import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with Matchers {

  "Monoid" should "fold seq of numbers" in {
    val numbers = (1 to 10).toSeq
    // Sum all the numbers
    MonoidOps.foldLeft(numbers) should be(55)

    // Sum all the numbers (Math trick :)
    MonoidOps.foldLeft(numbers) should be(10 * 11 / 2)
  }

  it should "fold seq of numbers (multiplying)" in {
    val numbers = (1 to 5).toSeq

    // Create a Monoid[Int] using multiply operation for integers
    implicit val multiplyingIntMonoid = new cats.Monoid[Int] {
      override def empty: Int = 1

      override def combine(x: Int, y: Int): Int = x * y
    }

    // Multiply all the numbers
    MonoidOps.foldLeft(numbers)(multiplyingIntMonoid) should be(120)
  }

  it should "fold seq of tuples of integers" in {
    val tuples = (1 to 10).toSeq.map(i => (i, i))
    // Sum all the first elements and second elements of the tuple
    MonoidOps.foldLeft(tuples) should be((55, 55))
  }

  it should "fold seq of tuples of integers and strings" in {
    val tuples = (1 to 4) zip ('a' to 'd').map(_.toString)
    // Sum all the first elements and second elements of the tuple
    MonoidOps.foldLeft(tuples) should be((10, "abcd"))
  }

  it should "merge a list of Map[String, Int] summing the values" in {
    val map1 = Map("a" -> 2, "b" -> 3)
    val map2 = Map("a" -> 3, "b" -> 1)
    val listOfMaps = List(map1, map2)

    val result = Map("a" -> 5, "b" -> 4)

    val ev = implicitly[Monoid[Map[String, Int]]]
    listOfMaps.foldLeft(ev.empty)(ev.combine) should be(result)

    MonoidOps.foldLeft(listOfMaps) should be(result)
  }

  "Monoid" should "provide empty for Option" in {
    Monoid[Option[Int]].empty should be(None)
  }

  it should "combine for Option" in {
    val o1 = 1.some
    val o2 = 2.some
    val result = 3.some

    val M = Monoid[Option[Int]]
    M.combine(o1, o2) should be(result)
    M.combine(None, o2) should be(o2)
    M.combine(o1, None) should be(o1)

    o1 |+| o2 should be(result)
    (None: Option[Int]) |+| o2 should be(o2)
    o1 |+| None should be(o1)
  }

  it should "provide empty for List" in {
    Monoid[List[Int]].empty should be(Nil)
  }

  it should "combine for List" in {
    val l1 = List(1, 2)
    val l2 = List(3, 4)
    val emptyList = Nil

    val M = Monoid[List[Int]]
    M.combine(l1, l2) should be(l1 ++ l2)
    M.combine(emptyList, l2) should be(l2)
    M.combine(l1, emptyList) should be(l1)
  }

}
