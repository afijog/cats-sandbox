package sandbox.typeclass

import cats.Functor
import cats.instances.list._
import cats.instances.option._
import cats.instances.try_._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Success, Try}

class FunctorSpec extends FlatSpec with Matchers {

  def addTwo(x: Int) = x + 2

  "Functor" should "compose over effects List[List]" in {
    val list = List(List(1, 2))
    val result = List(List(3, 4))
    Functor[List].compose[List].map(list)(_ + 2) should be(result)
  }

  it should "compose over effects List[Option]" in {
    val list = List(Some(1), Some(2), None)
    val result = List(Some(3), Some(4), None)
    Functor[List].compose[Option].map(list)(_ + 2) should be(result)
  }

  it should "compose over effects Try[List[Option]]" in {
    val list = Success(List(Some(1), Some(2), None))
    val result = Success(List(Some(3), Some(4), None))

    (Functor[Try].compose[List].compose[Option]).map(list)(_ + 2) should be(result)
    (Functor[Try].compose[List].compose[Option]).map(list)(addTwo) should be(result)
  }

  it should "lift and compose over effects Try[List[Option]]" in {
    val list = Success(List(Some(1), Some(2), None))
    val result = Success(List(Some(3), Some(4), None))

    (Functor[Try].compose[List].compose[Option]).lift(addTwo)(list) should be(result)
  }

  it should "map over an Option" in {
    val option = Option(1)
    val result = Option(3)

    val F = Functor[Option]

    // Use cats implementation
    F.map(option)(addTwo) should be(result)

    // Use my own implementation
    FunctorOps.optionFunctor.map(option)(addTwo) should be(result)
  }

  it should "map over a List" in {
    val list = List(1, 2, 3)
    val result = List(3, 4, 5)

    val F = Functor[List]

    // Use cats implementation
    F.map(list)(addTwo) should be(result)

    // Use my own implementation
    FunctorOps.listFunctor.map(list)(addTwo) should be(result)

  }

}
