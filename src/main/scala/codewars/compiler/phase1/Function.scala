package codewars.compiler.phase1

import bartosz.falgebras.FAlg.Fix
import codewars.compiler.AST.Expr
import codewars.compiler.ASTF.ExprF

object Function {

}

trait Function
case class Function1(args: List[String], expr: Expr) extends Function
case class Function2(args: List[String], expr: Fix[ExprF]) extends Function
