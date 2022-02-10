package bartosz.falgebras

import bartosz.falgebras.FAlg.{Fix, Functor, ListF}

object Anam {
  //!!!ANAMORPHISM!!!
  def ana[A, F[_]](coalg: A => F[A])(implicit F: Functor[F]): A => Fix[F] =
    a => {
      val fa: F[A] = coalg(a)
      val ft: F[Fix[F]] = F.fmap(ana(coalg))(fa)
      Fix(ft)
    }

  //***STREAM***
  trait StreamF[E, A]
  case class ConsStreamF[E, A](h: E, t: A) extends StreamF[E, A]
  case class NilStreamF[E, A]() extends StreamF[E, A]

  implicit def streamfFunctor[E] = new Functor[({type S[A] = StreamF[E, A]}) # S] {
    override def fmap[A, B](f: A => B): StreamF[E, A] => StreamF[E, B] = {
      case ConsStreamF(h, t) => ConsStreamF(h, f(t))
      case NilStreamF() => NilStreamF()
    }
  }

  def unfoldr[A, B]: (B => Option[(A, B)]) => B => List[A] = coalg => b => coalg(b) match {
    case None => Nil
    case Some((a, b)) => a::unfoldr(coalg)(b)
  }

  //***EVALUATION FUNCTIONS***

  //coalgebra for anamorphism
  def listOfSquares: List[Int] => StreamF[Int, List[Int]] = {
    case h::t => ConsStreamF(h*h, t)
    case _ => NilStreamF()
  }

  def era: List[Int] => StreamF[Int, List[Int]] = {
    case h::t =>
      def notdiv(p: Int)(n: Int): Boolean = n % p != 0

      ConsStreamF(h, t.filter(notdiv(h)))
  }

  def era2: List[Int] => Option[(Int, List[Int])] = {
    case h::t =>
      def notdiv(p: Int)(n: Int): Boolean = n % p != 0
      Some(h, t.filter(notdiv(h)))
    case Nil => None
  }

  //***EXTRA***

  //algebra for catamorphism
  def toListAlg[E]: StreamF[E, Fix[({type L[A] = ListF[E, A]})#L]] => Fix[({type L[A] = ListF[E, A]})#L] = {
    case ConsStreamF(h, t) => Fix(FAlg.ConsF(h, t))
  }
  //too fucking complicated with these anonymous type constructors
  def toList[E](input: Fix[({type S[A] = StreamF[E, A]})#S]): Fix[({type L[A] = ListF[E, A]})#L] = ???
//    val m = FAlg.cata2[Fix[({type S[A] = StreamF[E, A]})#S], Fix[({type L[A] = ListF[E, A]})#L]](toListAlg)(streamfFunctor)
//    m(input)

}
