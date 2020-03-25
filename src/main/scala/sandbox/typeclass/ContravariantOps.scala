package sandbox.typeclass

import cats.Contravariant

object ContravariantOps {
  trait SizeCalculator[A] {
    def size(a: A): Int
  }

  implicit val stringSizeCalculator = new SizeCalculator[String] {
    def size(a: String) = a.size
  }

  implicit val unitSizeCalculator = new SizeCalculator[Unit] {
    def size(a: Unit) = 0
  }

  implicit def productSizeCalculator[A, B](implicit
                                           fa: SizeCalculator[A],
                                           fb: SizeCalculator[B]) =
    new SizeCalculator[(A, B)] {
      override def size(a: (A, B)): Int = fa.size(a._1) + fb.size(a._2)
    }

  // Contravariant implicit class to allow 'enrichment' of basic types
  implicit class ContravariantOps[A](a: A) {
    def size(implicit sizeCalculator: SizeCalculator[A]) =
      sizeCalculator.size(a)
  }

  implicit val contravariantSizeCalculator: Contravariant[SizeCalculator] =
    new Contravariant[SizeCalculator] {
      override def contramap[A, B](fa: SizeCalculator[A])(f: B => A): SizeCalculator[B] =
        new SizeCalculator[B] {
          def size(b: B): Int = fa.size(f(b))
        }
    }
}
