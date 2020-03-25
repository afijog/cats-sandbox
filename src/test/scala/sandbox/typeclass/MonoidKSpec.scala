package sandbox.typeclass

import cats.MonoidK
import cats.instances.list._
import cats.instances.option._
import cats.syntax.option._
import org.scalatest.{FlatSpec, Matchers}

class MonoidKSpec extends FlatSpec with Matchers {
  "MonoidK" should "provide empty for Option" in {
    MonoidK[Option].empty should be(None)
  }

  it should "combine for Option" in {
    val o1 = 1.some
    val o2 = 2.some

    val M = MonoidK[Option]
    M.combineK(o1, o2) should be(o1)
    M.combineK(None, o2) should be(o2)
    M.combineK(o1, None) should be(o1)
  }

  it should "provide empty for List" in {
    MonoidK[List].empty should be(Nil)
  }

  it should "combine for List" in {
    val l1 = List(1, 2)
    val l2 = List(3, 4)
    val emptyList = Nil

    val M = MonoidK[List]
    M.combineK(l1, l2) should be(l1 ++ l2)
    M.combineK(emptyList, l2) should be(l2)
    M.combineK(l1, emptyList) should be(l1)
  }

}
