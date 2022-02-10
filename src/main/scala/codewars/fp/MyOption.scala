package codewars.fp

object MyOption {
  trait MyOption[A]
  case class MySome[A](a: A) extends MyOption[A]
  case object MyNone extends MyOption[Nothing]

  /*
   * Kliesly category for MyOption:
   *  1. Kleisli composition
   *  2. Identity (with regards to Kleisli composition)
   *  3. LAW: Kliesly composition is associative
   */
}
