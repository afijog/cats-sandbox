package sandbox.typeclass

import cats.instances.int._
import cats.instances.string._
import cats.instances.tuple._
import cats.syntax.bifoldable._
import cats.syntax.bifunctor._
import cats.syntax.either._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class BifunctorSpec extends FlatSpec with Matchers {
  "Bifunctor" should "bimap on Either" in {

    def divideAndMultiply(x: Int, y: Int) =
      Try(x / y).toEither.bimap(_.getMessage, _ * 1000.toLong)

    // bimap using right
    divideAndMultiply(4, 2) should be(Right[String, Long](2000l))

    divideAndMultiply(4, 0) should be(Left[String, Long]("/ by zero"))
  }

  it should "bimap on Tuple2" in {
    def process(x: Int) = (x, x + 1).bimap(_.toString, identity)
    process(4) should be(("4", 5))
  }

  it should "bifoldMap on Tuple2" in {
    // Needs implicit Monoid[String]
    def process(x: Int) = (x, x + 1).bifoldMap(_.toString, _.toString)
    process(4) should be("45")

    // Needs implicit Monoid[Int]
    def process2(x: Int) = (x, x + 1).bifoldMap(identity, identity)
    process2(4) should be(9)
  }

  it should "leftMap values" in {
    (1, 2) leftMap (_.toString) should be(("1", 2))
  }
}
