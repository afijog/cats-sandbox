package sandbox.model

import cats.data.NonEmptyList
import sandbox.model.UserValidation._

import org.scalatest.{FlatSpec, Matchers}

class UserSpec extends FlatSpec with Matchers {
  "UserValidation" should "parse numbers" in {
    parseNumber("5") should be(Right(5))
    parseNumber("five") should be(
      Left(NonEmptyList.one("five is not a number"))
    )
  }

  it should "validate ages" in {
    val age = 18
    validateAge(age) should be(Right(age))
    validateAge(5) should be(
      Left(NonEmptyList.one("Age must be greater than 18 and is 5"))
    )
  }

  it should "validate names" in {
    val name = User.defaultUser.name
    validateName(name) should be(Right(name))
    val shortName = "Jon"
    validateName(shortName) should be(
      Left(NonEmptyList.one(s"$shortName must have more than 5 characters"))
    )
  }
}
