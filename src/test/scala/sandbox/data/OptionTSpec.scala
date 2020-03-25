package sandbox.data

import cats.data.OptionT
import cats.instances.list._
import org.scalatest.{FlatSpec, Matchers}

class OptionTSpec extends FlatSpec with Matchers {
  "OptionT" should "map the inner data" in {

    // List[Option[Int]]
    val optionT = OptionT.liftF(List(1, 2, 3))

    // List[Option[String]]
    val result = OptionT.liftF(List("1", "2", "3"))

    // List[Option[Int]] => List[Option[String]]
    // Works on inner type
    optionT.map(_.toString) should be(result)
  }

  it should "semiflatMap" in {
    val optionT = OptionT.liftF(List(1, 2, 3))

    def process(x: Int) = List(x, -x) map { _.toString }

    val result = OptionT.liftF(List("1", "-1", "2", "-2", "3", "-3"))

    optionT.semiflatMap(process) should be(result)
  }

}
