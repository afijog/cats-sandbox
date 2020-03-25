package sandbox.typeclass

import cats.Monoid

object MonoidOps {

  // Reinvent foldLeft using a Monoid
  def foldLeft[A](elements: Seq[A])(implicit ev: Monoid[A]): A =
    elements.foldLeft(ev.empty)(ev.combine)
}