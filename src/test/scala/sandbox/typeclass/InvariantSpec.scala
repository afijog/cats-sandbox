package sandbox.typeclass

import java.util.Date

import cats.Semigroup
import cats.instances.invariant._
import cats.instances.long._
import cats.instances.double._
import cats.syntax.invariant._
import cats.syntax.semigroup._
import org.scalatest.{FlatSpec, Matchers}
import sandbox.model.Money

class InvariantSpec extends FlatSpec with Matchers {
  "Invariant" should "help to derive Semigroups" in {
    // Helper functions
    def longToDate(l: Long) = new Date(l)
    def dateToLong(d: Date) = d.getTime

    val S = Semigroup[Long]

    implicit val D: Semigroup[Date] = S.imap(longToDate)(dateToLong)

    val date1 = new Date()
    date1.setTime(5)

    val date2 = new Date()
    date2.setTime(10)

    D.combine(date1, date1) should be(date2)
    date1 |+| date1 should be(date2)
  }
  it should "help to derive Semigroups (II)" in {
    // Helper functions
    val doubleToMoney : Double => Money = Money(_)

    val moneyToDouble: Money => Double = _.amount

    val S = Semigroup[Double]

    implicit val M: Semigroup[Money] = S.imap(doubleToMoney)(moneyToDouble)

    val money1 = Money(5)

    val money2 = Money(10)

    M.combine(money1, money1) should be(money2)
    money1 |+| money1 should be(money2)
  }
}
