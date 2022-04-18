package codewars.compiler.phase2

import bartosz.falgebras.FAlg.{Fix, cata}
import codewars.compiler.ASTF._

object Simplify {
  def simplifyExpr(e: Fix[ExprF]): Fix[ExprF] = {
    def simplifyAlg: ExprF[Fix[ExprF]] => Fix[ExprF] = {
      case DivF(Fix(NumF(l)), Fix(NumF(r))) => num(l / r)
      case AddF(Fix(NumF(l)), Fix(NumF(r))) => num(l + r)
      case MulF(Fix(NumF(l)), Fix(NumF(r))) => num(l * r)
      case SubF(Fix(NumF(l)), Fix(NumF(r))) => num(l - r)
      case x => Fix(x) //"identity"
    }

    val g: Fix[ExprF] => Fix[ExprF] = cata(simplifyAlg) //functor Functor[ExprF] implicitly imported and applied

    g(e) //uses catamorphism on the expression tree to produce a new simplified expression tree
  }
}
