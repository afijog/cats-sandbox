package sandbox.typeclass

import cats.instances.option._
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.option._
import org.scalatest.{FlatSpec, Matchers}

class ApplySpec extends FlatSpec with Matchers {
  "Apply" should "preserve data with productLeft and productRight (Option)" in {

    val data1 = "data1".some
    val data2 = "data2".some

    // productRight
    (data1 *> data2) should be(data2)
    (data2 *> data1) should be(data1)

    // productLeft
    (data1 <* data2) should be(data1)
    (data2 <* data1) should be(data2)
  }

  it should "preserve data with productLeft and productRight (Either)" in {
    type EEither[A] = Either[Int, A]

    val data1: EEither[String] = "data1".asRight[Int]
    val data2: EEither[String] = "data2".asRight[Int]
    val error: EEither[String] = 200.asLeft[String]

    // productRight
    (data1 *> data2) should be(data2)
    (data2 *> data1) should be(data1)
    (data1 *> error) should be(error)
    (error *> data1) should be(error)

    // productLeft
    (data1 <* data2) should be(data1)
    (data2 <* data1) should be(data2)
    (data1 <* error) should be(error)
    (error <* data1) should be(error)
  }
}
