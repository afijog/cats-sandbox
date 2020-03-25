package sandbox.typeclass

import cats.ContravariantMonoidal
import sandbox.typeclass.ContravariantOps.SizeCalculator

object ContravariantMonoidalOps {

  implicit val contravariantMonoidalSizeCalculator : ContravariantMonoidal[SizeCalculator] =
    new ContravariantMonoidal[SizeCalculator] {
      override def unit: SizeCalculator[Unit] = ContravariantOps.unitSizeCalculator

      override def product[A, B](
        fa: SizeCalculator[A],
        fb: SizeCalculator[B]
      ): SizeCalculator[(A, B)] = ContravariantOps.productSizeCalculator(fa, fb)

      override def contramap[A, B](fa: SizeCalculator[A])(f: B => A): SizeCalculator[B] =
        ContravariantOps.contravariantSizeCalculator.contramap(fa)(f)
    }
}
