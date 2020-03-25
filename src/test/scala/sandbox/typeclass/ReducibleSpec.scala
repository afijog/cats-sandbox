package sandbox.typeclass

import cats.Reducible
import cats.data.NonEmptyList
import cats.instances.int._
import org.scalatest.{FlatSpec, Matchers}

class ReducibleSpec extends FlatSpec with Matchers {

  def addTwo(x: Int): Int = x + 2
  def greaterThan2(x: Int): Boolean = x > 2
  def equalOrGreaterThan2(x: Int): Boolean = x >= 2

  "Reducible" should "reduce NonEmptyList[Int]" in {
    val list = NonEmptyList.of(1, 2)

    val reducibleList = Reducible[NonEmptyList]

    reducibleList.fold(list)should be(3)

    reducibleList.foldMap(list)(addTwo) should be(7)

    reducibleList.find(list)(greaterThan2) should be(None)
    reducibleList.find(list)(equalOrGreaterThan2) should be(Some(2))

    reducibleList.exists(list)(greaterThan2) should be (false)
    reducibleList.exists(list)(equalOrGreaterThan2) should be (true)

    reducibleList.forall(list)(greaterThan2) should be (false)
    reducibleList.forall(list)(equalOrGreaterThan2) should be (false)

    reducibleList.filter_(list)(equalOrGreaterThan2) should be (List(2))

    reducibleList.isEmpty(NonEmptyList.of(1,2)) should be (false)
    reducibleList.isEmpty(NonEmptyList.of(1)) should be (false)

  }
}
