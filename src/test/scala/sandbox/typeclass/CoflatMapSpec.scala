package sandbox.typeclass

import cats.CoflatMap
import cats.instances.list._
import cats.instances.option._
import org.scalatest.{FlatSpec, Matchers}

class CoflatMapSpec extends FlatSpec with Matchers {
  "CoflatMap" should "do the opposite of flatMap for Option" in {
    val option = Option(1)
    val result = Option(2)

    val C = CoflatMap[Option]

    def inc(x: Option[Int]) = x.map(_ + 1).getOrElse(0)

    // Use cats implementation
    C.coflatMap(option)(inc) should be(result)

    // Use my own implementation
    CoflatMapOps.optionCoflatMap.coflatMap(option)(inc) should be(result)

    // Use cats implementation
    C.coflatMap(None)(inc) should be(None)

    // Use my own implementation
    CoflatMapOps.optionCoflatMap.coflatMap(None)(inc) should be(None)
  }

  it should "do the opposite of flatMap for List" in {
    val list = List(1, 2, 3)
    val result = List(List(1, 2, 3), List(2, 3), List(3))
    val resultAsLengths = List(3, 2, 1)

    val C = CoflatMap[List]

    // Use cats implementation to generate lists
    C.coflatMap(list)(identity) should be(result)

    // Use my own implementation to generate lists
    CoflatMapOps.listCoflatMap.coflatMap(list)(identity) should be(result)

    // Use my own implementation to calculate length of lists
    CoflatMapOps.listCoflatMap.coflatMap(list)(_.length) should be(resultAsLengths)
  }
}
