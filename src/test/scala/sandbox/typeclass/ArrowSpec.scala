package sandbox.typeclass

import cats.Applicative
import cats.arrow.Arrow
import cats.data.Kleisli
import cats.instances.function._
import cats.instances.option._
import cats.syntax.arrow._
import cats.syntax.compose._
import org.scalatest.{FlatSpec, Matchers}

class ArrowSpec extends FlatSpec with Matchers {
  def tuple[F[_, _]: Arrow, A, B, C](fab: F[A, B],
                                     fac: F[A, C]): F[A, (B, C)] = {
    val createTuple = (a: A) => (a, a)
    val createTupleArrow: F[A, (A, A)] = Arrow[F].lift(createTuple)

    val splitArrow: F[(A, A), (B, C)] = fab split fac

    // Create the tuple, andThen (>>>), execute the split
    createTupleArrow >>> splitArrow
    // You can also use
    // createTupleArrow andThen splitArrow

    // Or with compose
    // splitArrow compose createTupleArrow
    // splitArrow <<< createTupleArrow
  }

  "Arrow" should "split over a tuple" in {
    val A = Applicative[Option]

    val inc: Option[Int] => Option[Int] = A.lift({ i: Int =>
      i + 1
    })
    val toStr: Option[Int] => Option[String] = A.lift({ i: Int =>
      i.toString
    })

    val incAndToStringTupled: Option[Int] => (Option[Int], Option[String]) =
      tuple(inc, toStr)

    val simpleResult = (Some(2), Some("1"))
    val result = (A.pure(2), A.pure("1"))

    simpleResult should be(result)
    incAndToStringTupled(A.pure(1)) should be(result)

    // Use Applicative.pure to lift the integer
    val incAndToStringTupledFromInt: Int => (Option[Int], Option[String]) =
      i => incAndToStringTupled.apply(A.pure(i))

    incAndToStringTupledFromInt(1) should be(result)
  }

  "Kleisli" should "compose" in {
    type OKleisli[A, B] = Kleisli[Option, A, B]

    val headK: OKleisli[List[Int], Int] = Kleisli((_: List[Int]).headOption)
    val lastK: OKleisli[List[Int], Int] = Kleisli((_: List[Int]).lastOption)

    val l = (1 to 10).toList

    headK.run(l) should be(Some(1))
    lastK.run(l) should be(Some(10))

    implicit val A = Arrow[OKleisli]

    val tupledKleisli: OKleisli[List[Int], (Int, Int)] = tuple(headK, lastK)
    val sumTupleKleisli: OKleisli[(Int, Int), Int] =
      A.lift(((_: Int) + (_: Int)).tupled)

    val combinedKleisli = tupledKleisli andThen sumTupleKleisli

    combinedKleisli.run(l) should be(Some(11))
  }
}
