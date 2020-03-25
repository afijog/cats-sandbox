package sandbox.typeclass

import cats.SemigroupK
import cats.instances.list._
import cats.instances.option._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.semigroupk._
import org.scalatest.{FlatSpec, Matchers}

class SemigroupKSpec extends FlatSpec with Matchers {
  "SemigroupK" should "combine higher order constructors (List)" in {
    val xs1 = List("a" -> 2, "b" -> 3)
    val xs2 = List("a" -> 3, "b" -> 1)

    // SemigroupK add all elements but does not merge like Semigroup
    val result = List("a" -> 2, "b" -> 3, "a" -> 3, "b" -> 1)

    // Infix operator
    (xs1 <+> xs2) should be(result)

    // Enrich the List type with implicit class
    xs1.combineK(xs2) should be(result)

    // A little more verbose
    SemigroupK[List].combineK(xs1, xs2) should be(result)
  }

  it should "combine higher order constructors (Option)" in {
    val o1 = Option(2)
    val o2 = Some(3): Option[Int]
    val oNone: Option[Int] = None

    // SemigroupK add all elements but does not merge like Semigroup
    val result = Option(2)

    // Infix operator
    (o1 <+> o2) should be(result)

    // Enrich the Option type with implicit class
    o1.combineK(o2) should be(result)

    // A little more verbose
    SemigroupK[Option].combineK(o1, o2) should be(result)

    (o1 <+> oNone) should be(o1)
    (oNone <+> o1) should be(o1)
    (oNone <+> oNone) should be(oNone)

    (o1 <+> o2) should be(o1)
    (o2 <+> o1) should be(o2)
  }

  "SemigroupK" should "combine and apply functions" in {
    val data = (1 to 3).toVector

    // format: off
    val inc = { i: Int => i + 1 }.pure[Vector]
    val times2 = { i: Int => i + i }.pure[Vector]
    val sq = { i: Int => i * i}.pure[Vector]
    // format: on

    def addAndMultiply(i: Int) =
      i -> ((inc <+> times2 <+> sq) ap i.pure[Vector])

    val processedData = (addAndMultiply _).pure[Vector] ap data
    val result =
      Vector((1, Vector(2, 2, 1)), (2, Vector(3, 4, 4)), (3, Vector(4, 6, 9)))

    processedData should be(result)
  }
}
