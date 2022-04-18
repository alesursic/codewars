package codewars.compiler

object AST {
  trait Expr
  case class Add(e: Expr, t: Term) extends Expr
  case class Sub(e: Expr, t: Term) extends Expr

  trait Term extends Expr
  case class Mul(t: Term, f: Factor) extends Term
  case class Div(t: Term, f: Factor) extends Term

  trait Factor extends Term
  case class Num(v: Int) extends Factor
  case class Var(v: String) extends Factor
  case class In(e: Expr) extends Factor //to avoid cyclic dependency
}
