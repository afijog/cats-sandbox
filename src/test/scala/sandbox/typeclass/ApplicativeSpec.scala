package sandbox.typeclass

import cats.Applicative
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.option._
import org.scalatest.{FlatSpec, Matchers}
import sandbox.model.User

class ApplicativeSpec extends FlatSpec with Matchers {
  "Applicative" should "lift value into effect" in {
    val F = Applicative[Option]
    F.pure(2) should be(Some(2))
    F.pure(2) should be(Some(2): Option[Int])

    2.some should be(Some(2))
  }

  "Applicative" should "map values" in {
    val name = User.name
    val email = User.email

    val A = Applicative[Option]
    val result = A.pure(User.defaultUser)

    A.map2(A.pure(name), A.pure(email))(User.apply) should be(result)

    (A.pure(name), A.pure(email)) mapN (User.apply) should be(result)

    (name.some, email.some) mapN (User.apply) should be(result)
  }

  it should "map values (II)" in {
    (1.some, 2.some) mapN (_ + _) should be(3.some)
  }

  it should "product values" in {
    val name = User.name
    val email = User.email

    val A = Applicative[Option]
    val result = A.pure((name, email))

    A.product(A.pure(name), A.pure(email)) should be(result)
  }
}
