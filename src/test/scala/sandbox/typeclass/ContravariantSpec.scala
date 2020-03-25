package sandbox.typeclass

import cats.syntax.contravariant._
import cats.syntax.show._
import cats.Show
import org.scalatest.{FlatSpec, Matchers}
import sandbox.model.{User, UserWithData}
import sandbox.typeclass.ContravariantOps.SizeCalculator

class ContravariantSpec extends FlatSpec with Matchers {
  "Contravariant" should "contramap with Show" in {

    implicit val showUser: Show[User] = Show.show(user => s"I am ${user.name}")
    implicit val showUserWithData: Show[UserWithData] =
      showUser.contramap(_.user)

    User.defaultUser.show should be("I am James")
    User.defaultUserWithData.show should be("I am James")
  }

  it should "contramap with own type class" in {
    implicit val stringSizeCalculator: SizeCalculator[String] =
      ContravariantOps.stringSizeCalculator

    implicit val userSizeCalculator: SizeCalculator[User] =
      stringSizeCalculator.contramap(_.name)

    implicit val userWithDataSizeCalculator: SizeCalculator[UserWithData] =
      stringSizeCalculator.contramap(_.user.name)

    val user = User.defaultUser
    val userWithData = User.defaultUserWithData

    stringSizeCalculator.size(user.name) should be(5)
    userSizeCalculator.size(user) should be(5)
    userWithDataSizeCalculator.size(userWithData) should be(5)
  }
}
