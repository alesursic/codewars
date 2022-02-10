package bartosz.falgebras

import bartosz.falgebras.FAlg.Functor

object Ring {
  sealed trait RingF[E, A]
  case class ZeroF[E, A]() extends RingF[E, A]
  case class OneF[E, A]() extends RingF[E, A]
  case class ValF[E, A](e: E) extends RingF[E, A]
  case class AddF[E, A](l: A, r: A) extends RingF[E, A]
  case class MulF[E, A](l: A, r: A) extends RingF[E, A]
  case class NegF[E, A](x: A) extends RingF[E, A]

  def ringFunctor[E] = new Functor[({type R[A] = RingF[E, A]})#R] {
    override def fmap[A, B](f: A => B): RingF[E, A] => RingF[E, B] = {
      case ZeroF() => ZeroF()
      case OneF() => OneF()
      case ValF(p) => ValF(p)
      case AddF(l, r) => AddF(f(l), f(r))
      case MulF(l, r) => MulF(f(l), f(r))
      case NegF(x) => NegF(f(x))
    }
  }
}
