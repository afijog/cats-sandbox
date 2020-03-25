package sandbox.typeclass

import cats.Semigroup
import cats.instances.int._
import cats.instances.list._
import cats.instances.map._
import cats.instances.option._
import cats.syntax.monoid._
import org.scalatest.{FlatSpec, Matchers}

class SemigroupSpec extends FlatSpec with Matchers {
  "Semigroup" should "combine a Map[String, Int] summing the values by keys" in {
    val map1 = Map("a" -> 2, "b" -> 3)
    val map2 = Map("a" -> 3, "b" -> 1)

    val result = Map("a" -> 5, "b" -> 4)

    // Infix operator
    (map1 |+| map2) should be(result)

    // Enrich the Map type with implicit class
    map1.combine(map2) should be(result)

    // A little more verbose
    Semigroup[Map[String, Int]].combine(map1, map2) should be(result)
  }

  it should "combine Option values" in {
    val o1 = Option(2)
    val o2 = Some(3): Option[Int]
    val oNone: Option[Int] = None

    val result = Option(5)

    // Infix operator
    (o1 |+| o2) should be(result)

    // Enrich the Option type with implicit class
    o1.combine(o2) should be(result)

    // A little more verbose
    Semigroup[Option[Int]].combine(o1, o2) should be(result)

    (o1 |+| oNone) should be(o1)
    (oNone |+| o1) should be(o1)
    (oNone |+| oNone) should be(oNone)

    (o1 |+| o2) should be(result)
    (o2 |+| o1) should be(result)
  }

  it should "combine List values" in {
    val xs1 = List(1, 2)
    val xs2 = List(3, 4)
    val xs3: List[Int] = Nil

    val result = List(1, 2, 3, 4)
    val result2 = List(3, 4, 1, 2)

    // Infix operator
    (xs1 |+| xs2) should be(result)

    // Enrich the Option type with implicit class
    xs1.combine(xs2) should be(result)

    // A little more verbose
    Semigroup[List[Int]].combine(xs1, xs2) should be(result)

    (xs1 |+| xs3) should be(xs1)
    (xs3 |+| xs1) should be(xs1)
    (xs3 |+| xs3) should be(xs3)

    (xs1 |+| xs2) should be(result)
    (xs2 |+| xs1) should be(result2)
  }
}
