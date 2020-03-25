package sandbox.model

import cats.data.{EitherNel, NonEmptyList}

import scala.util.Try

case class User(name: String, email: String, age: Int)

object User {
  def apply(name: String, age: Int): User = User(name, "", age)
  def apply(name: String, email: String): User = User(name, email, 0)

  val name = "James"
  val email = "james@acme.org"

  val defaultUser = User(name, email)
  val defaultUserWithData = UserWithData(defaultUser)
}

object UserValidation {
  def parseNumber(s: String): EitherNel[String, Int] = {
    Try(s.toInt).toEither.left
      .map(_ => NonEmptyList.one(s"$s is not a number"))
  }

  def validateAge(a: Int): EitherNel[String, Int] = {
    val minAge = 18
    if (a >= minAge) Right(a)
    else Left(NonEmptyList.one(s"Age must be greater than $minAge and is $a"))
  }

  def validateName(name: String): EitherNel[String, String] = {
    val nChars = 5
    if (name.length >= nChars) Right(name)
    else Left(NonEmptyList.one(s"$name must have more than $nChars characters"))
  }
}

case class UserWithData(user: User, data: String = "")
