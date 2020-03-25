package sandbox.typeclass

object CoflatMapOps {
  // Reinvent CoflatMap trait
  trait CoflatMap[F[_]] {
    def coflatMap[A, B](fa: F[A])(f: F[A] => B): F[B]
  }

  // Reinvent CoflatMap implementation for Option
  val optionCoflatMap: CoflatMap[Option] = new CoflatMap[Option] {
    def coflatMap[A, B](fa: Option[A])(f: Option[A] => B): Option[B] =
      fa match {
        case Some(x) => Some(f(fa))
        case None    => None
      }
  }

  // Reinvent CoflatMap implementation for List
  val listCoflatMap: CoflatMap[List] = new CoflatMap[List] {
    def coflatMap[A, B](fa: List[A])(f: List[A] => B): List[B] = fa match {
      case h :: t => f(fa) :: coflatMap(t)(f)
      case Nil    => Nil
    }
  }
}
