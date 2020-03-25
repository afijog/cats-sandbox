package sandbox.typeclass

object FunctorOps {
  // Reinvent Functor trait
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  // Reinvent Functor implementation for Option
  val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(x) => Some(f(x))
        case None    => None
      }
  }

  // Reinvent Functor implementation for List
  val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
      case h :: t => f(h) :: map(t)(f)
      case Nil    => Nil
    }
  }
}
