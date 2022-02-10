package bartosz.falgebras

import bartosz.falgebras.FAlg.Functor

object PolyRing {
  sealed trait RingF[A]
  case class ZeroF[A]() extends RingF[A]
  case class OneF[A]() extends RingF[A]
  case class ValF[A](cs: List[Int]) extends RingF[A]
  case class ScMulF[A](cs: List[Int], sc: (Int, Int)) extends RingF[A]
  case class AddF[A](l: A, r: A) extends RingF[A]
  case class MulF[A](l: A, r: A) extends RingF[A]
  case class NegF[A](x: A) extends RingF[A]

  case class Polynom(cs: List[Int])

  //****Helpers****
  object ZeroPolynom extends Polynom(List(0, 0, 0, 0, 0))
  val FixedZeroPolynomF: FAlg.Fix[RingF] = FAlg.Fix(ZeroF())
  type FixRingF = FAlg.Fix[RingF]
  def fixValF(cs: List[Int]): FixRingF = FAlg.Fix(ValF(cs))
  def fixMulF(l: FixRingF, r: FixRingF): FixRingF = FAlg.Fix(MulF(l, r))
  def fixAddF(l: FixRingF, r: FixRingF): FixRingF = FAlg.Fix(AddF(l, r))
  //The DSL:
  implicit def toFixRingfOps(x: FixRingF) = new FixRingfOps(x)
  class FixRingfOps(x: FixRingF) {
    def *(y: FixRingF): FixRingF = fixMulF(x, y)
    def :+:(y: FixRingF): FixRingF = fixAddF(x, y) //+ "reserved" by StringOps
  }
  //****End of helpers****

  implicit val ringfFunctor = new Functor[RingF] {
    override def fmap[A, B](f: A => B): RingF[A] => RingF[B] = {
      case ZeroF() => ZeroF()
      case OneF() => OneF()
      case ValF(p) => ValF(p)
      case ScMulF(cs, sc) => ScMulF(cs, sc)
      case AddF(l, r) => AddF(f(l), f(r))
      case MulF(l, r) => MulF(f(l), f(r))
      case NegF(x) => NegF(f(x))
    }
  }

  //(a0 + b0 + c0) * (a1 + b1 + c1) == a0a1 + b0a1 + c0a1 + a0b1 + b0b1 + c0b1 + a0c1 ==
  //(a0 + b0 + c0)*a1 + (a0 + b0 + c0)*b1 + (a0 + b0 + c0)*c1
  //another way:
  //      polysToAdd.fold(ZeroPolynom)({case (acc, a) => evalP(AddF(acc, a))}) //fucking recursion..
  def evalP: RingF[Polynom] => Polynom = {
    case ZeroF() => ZeroPolynom
    case OneF() => Polynom(List(1, 1, 1, 1, 1))
    case ValF(p) => Polynom(p)
    case ScMulF(cs, (coef, pow)) => Polynom(List.fill(pow)(0) ++ cs.map(coef*_))
    case AddF(Polynom(lcs), Polynom(rcs)) => Polynom(lcs.zip(rcs).map({case (a, b) => a + b}))
    case MulF(Polynom(lcs), Polynom(rcs)) => {
      val scalarMultiplications: List[FAlg.Fix[PolyRing.RingF]] = rcs.zipWithIndex.map(pair => FAlg.Fix(ScMulF(lcs, pair)))
      val newExpr: FAlg.Fix[RingF] = scalarMultiplications
        .foldLeft(FixedZeroPolynomF)({case (acc, a) => FAlg.Fix(AddF(acc, a))})

      FAlg.cata(evalP)(ringfFunctor)(newExpr)
    }
    case NegF(Polynom(cs)) => Polynom(cs.map(-_))
  }
}
