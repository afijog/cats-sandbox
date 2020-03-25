package sandbox.typeclass

import cats.Monad
import cats.instances.list._
import org.scalatest.{FlatSpec, Matchers}

class MonadSpec extends FlatSpec with Matchers {
  "Monad" should "flatten effects" in {
    val list = List(List(1, 2))
    val result = List(1, 2)

    Monad[List].flatten(list) should be(result)
  }

  it should "perform if with effects" in {
    val list = List(true, true, false, true, false)
    val result = List(1, 2, 1, 2, 3, 4, 1, 2, 3, 4)

    Monad[List].ifM(list)(List(1, 2), List(3, 4)) should be(result)
  }
}
