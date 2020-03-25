package sandbox.typeclass

import cats.Comonad
import cats.data.NonEmptyList
import cats.syntax.comonad._
import org.scalatest.{FlatSpec, Matchers}

class ComonadSpec extends FlatSpec with Matchers {
  "Comonad" should "be able to extract data from non empty data" in {
    val value1 = NonEmptyList.of(1,2,3)
    val value2 = NonEmptyList.of(1)
    val result = 1

    Comonad[NonEmptyList].extract(value1) should be(result)
    value1.extract should be(result)

    Comonad[NonEmptyList].extract(value2) should be(result)
    value2.extract should be(result)
  }
}
