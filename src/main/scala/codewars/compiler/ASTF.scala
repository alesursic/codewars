package codewars.compiler

import bartosz.falgebras.FAlg.{Fix, Functor}

object ASTF {
  //Data types:
  trait ExprF[A]
  trait TermF[A] extends ExprF[A]
  trait FactorF[A] extends TermF[A]

  case class NumF[A](v: Int) extends FactorF[A]
  case class VarF[A](v: String) extends FactorF[A]
  case class InF[A](e: A) extends FactorF[A]

  case class MulF[A](t: A, f: A) extends TermF[A]
  case class DivF[A](t: A, f: A) extends TermF[A]

  case class AddF[A](e: A, t: A) extends ExprF[A]
  case class SubF[A](e: A, t: A) extends ExprF[A]

  //Helpers:

  def num[A](v: Int): Fix[ExprF] = Fix(NumF(v))
  def varr[A](v: String): Fix[ExprF] = Fix(VarF(v))
  def in[A](e: Fix[ExprF]): Fix[ExprF] = Fix(InF(e))
  //todo: use actual sub-types (parameterize them) instead of Fix[ExprF]
  def mul[A](t: Fix[ExprF], f: Fix[ExprF]): Fix[ExprF] = Fix(MulF(t, f))
  def div[A](t: Fix[ExprF], f: Fix[ExprF]): Fix[ExprF] = Fix(DivF(t, f))
  def add[A](e: Fix[ExprF], t: Fix[ExprF]): Fix[ExprF] = Fix(AddF(e, t))
  def sub[A](e: Fix[ExprF], t: Fix[ExprF]): Fix[ExprF] = Fix(SubF(e, t))

  implicit val exprfFunctor = new Functor[ExprF] {
    override def fmap[A, B](g: A => B): ExprF[A] => ExprF[B] = {
      case NumF(v) => NumF(v)
      case VarF(v) => VarF(v)
      case InF(e) => InF(g(e))
      case DivF(t, f) => DivF(g(t), g(f))
      case MulF(t, f) => MulF(g(t), g(f))
      case AddF(e, t) => AddF(g(e), g(t))
      case SubF(e, t) => SubF(g(e), g(t))
    }
  }
}
