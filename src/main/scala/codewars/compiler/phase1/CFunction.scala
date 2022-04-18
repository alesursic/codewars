package codewars.compiler.phase1

import bartosz.falgebras.FAlg.Fix
import codewars.compiler.AST.Expr
import codewars.compiler.ASTF.ExprF

object CFunction {}

trait CFunction
case class CFunction1(args: List[String], expr: Expr) extends CFunction
case class CFunction2(args: List[String], expr: Fix[ExprF]) extends CFunction
