package sandbox.typeclass

import cats.ContravariantMonoidal
import org.scalatest.{FlatSpec, Matchers}
import sandbox.model.User

class ContravariantMonoidalSpec extends FlatSpec with Matchers {
  "ContravariantMonoidal" should "have pure" in {

    import ContravariantOps._
    import ContravariantMonoidalOps._

    implicitly[ContravariantMonoidal[SizeCalculator]].unit.size(()) should be(0)

    contravariantMonoidalSizeCalculator.unit.size(()) should be(0)
    ().size should be(0)
  }

  it should "have product" in {
    import ContravariantOps._

    val user = User.defaultUser
    val result = user.name.length + user.email.length

    (user.name, user.email).size should be(result)
  }
}
