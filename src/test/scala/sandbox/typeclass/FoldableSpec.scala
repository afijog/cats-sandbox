package sandbox.typeclass

import cats.Foldable
import cats.instances.int._
import cats.instances.list._
import org.scalatest.{FlatSpec, Matchers}

class FoldableSpec extends FlatSpec with Matchers {

  def addTwo(x: Int): Int = x + 2
  def greaterThan2(x: Int): Boolean = x > 2
  def equalOrGreaterThan2(x: Int): Boolean = x >= 2

  "Foldable" should "fold List[Int]" in {
    val list = List(1, 2)

    val foldableList = Foldable[List]

    foldableList.fold(list)should be(3)

    foldableList.foldMap(list)(addTwo) should be(7)

    foldableList.foldK(List(list, list)) should be(List(1, 2, 1, 2))

    foldableList.find(list)(greaterThan2) should be(None)
    foldableList.find(list)(equalOrGreaterThan2) should be(Some(2))

    foldableList.exists(list)(greaterThan2) should be (false)
    foldableList.exists(list)(equalOrGreaterThan2) should be (true)

    foldableList.forall(list)(greaterThan2) should be (false)
    foldableList.forall(list)(equalOrGreaterThan2) should be (false)

    foldableList.filter_(list)(equalOrGreaterThan2) should be (List(2))

    foldableList.isEmpty(List(1,2)) should be (false)
    foldableList.isEmpty(List()) should be (true)
    foldableList.isEmpty(List.empty) should be (true)

  }
}
