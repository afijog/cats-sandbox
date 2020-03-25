package sandbox.typeclass

import cats.instances.list._
import cats.instances.option._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.{Applicative, Traverse}
import org.scalatest.{FlatSpec, Matchers}

class TraverseSpec extends FlatSpec with Matchers {
  val T = Traverse[List]
  val A = Applicative[Option]

  "Traverse" should "traverse swapping effects" in {
    val list = List(1, 2, 3)
    val result = Option(List(1, 2, 3))

    T.traverse(list)(A.pure) should be(result)
  }

  it should "traverse swapping effects (II)" in {
    val list = List(Some(1), None)
    val result = None

    T.traverse(list)(identity) should be(result)
  }

  it should "sequence as traverse with identity" in {
    val list = List(Some(1), None)

    T.traverse(list)(identity) should be(T.sequence(list))
  }

  it should "flat traverse" in {
    def process(x: Int): List[Option[Int]] = List(x.some, (x + 1).some)

    (1.some traverse process) should be(List(1.some.some, 2.some.some))
    (1.some flatTraverse process) should be(List(1.some, 2.some))
  }
}
