package codewars.compiler.phase1

import bartosz.falgebras.FAlg.Fix
import codewars.compiler.ASTF._

import scala.annotation.tailrec

/*
    function   ::= '[' arg-list ']' expression

    arg-list   ::= /* nothing */
                 | variable arg-list

    expression ::= term
                 | expression '+' term
                 | expression '-' term

    term       ::= factor
                 | term '*' factor
                 | term '/' factor

    factor     ::= number
                 | variable
                 | '(' expression ')'
 */
object AstfDeserialization {
  def apply(program: String): CFunction = AstfDeserialization().deserialize(program)

  case class Acc(exprF: Fix[ExprF], varAcc: List[Char]) {
    def ::(head: Char) = Acc(exprF, head :: varAcc)
  }
  def emptyAcc = Acc(null, Nil)
}

case class AstfDeserialization() {
  def deserialize(program: String): CFunction = {
    val parts = program.split("\\[")(1).split("\\]")

    val args = parts(0).split(" ").toList.tail
    val expr = evalRunner(null, parts(1).toList.filter(_ != ' '))

    CFunction2(args, expr)
  }

  /*
   * factor    ::= number
                 | variable
                 | '(' expression ')'
   */
  @tailrec
  private[this] def evalFactor(left: List[Char], input: List[Char]): (Fix[ExprF], List[Char]) = {
    def terminate: (Fix[ExprF], List[Char]) = {
      val isNumber = left.forall(_.isDigit)
      val leftVal = left.reverse.mkString

      (if (isNumber) num(leftVal.toInt) else varr(leftVal), input)
    }

    if (input.nonEmpty)
      println("factor: " + input.head)

    input match {
      //[terminal cases]:
      case Nil => terminate
      case op::_ if List('*', '+', '/', '-', ')').contains(op) => terminate

      //[recursive cases]:
      case '('::tail => evalExpr(null, tail) match {
        case (expr, leftover) => (in(expr), leftover)
      }

      //alphanumeric char
      case char::tail => evalFactor(char::left, tail) //re-evaluation
    }
  }

  /*
   * term       ::= factor
                 | term '*' factor
                 | term '/' factor
   */
  @tailrec
  private[this] def evalTerm(left: Fix[ExprF], input: List[Char]): (Fix[ExprF], List[Char]) = {
    if (input.nonEmpty)
      println("term: " + input.head)

    input match {
      //[terminal cases]:
      case Nil => (left, Nil)
      case op::_ if List('+', '-', ')').contains(op) => (left, input)

      //[recursive cases]:
      //right-hand
      case '*'::tail => evalFactor(Nil, tail) match {
        case (right, leftover) => (mul(left, right), leftover)
      }
      case '/'::tail => evalFactor(Nil, tail) match {
        case (right, leftover) => (div(left, right), leftover)
      }

      //left-hand (alphanumeric char, '(')
      case _ => evalFactor(Nil, input) match {
        case (newLeft, newInput) => evalTerm(newLeft, newInput) //re-evaluation
      }
    }
  }

  /*
   * expression ::= term
                 | expression '+' term
                 | expression '-' term
   */
  @tailrec
  private[this] def evalExpr(left: Fix[ExprF], input: List[Char]): (Fix[ExprF], List[Char]) = {
    if (input.nonEmpty)
      println("expr: " + input.head)

    input match {
      //[terminal cases]:
      case Nil => (left, Nil)
      case ')'::tail => (left, tail)

      //[recursive cases]:
      //right-hand
      case '+'::tail => evalTerm(null, tail) match {
        case (right, leftover) => (add(left, right), leftover)
      }
      case '-'::tail => evalTerm(null, tail) match {
        case (right, leftover) => (sub(left, right), leftover)
      }

      //left-hand (alphanumeric char, '*', '/', '(')
      case _ => evalTerm(left, input) match {
        case (newLeft, newInput) => evalExpr(newLeft, newInput) //re-evaluation
      }
    }
  }

  //keeps parsing until the end of input is reached
  @tailrec
  private[this] def evalRunner(left: Fix[ExprF], input: List[Char]): Fix[ExprF] = {
    evalExpr(left, input) match {
      case (newLeft, Nil) => newLeft
      case (newLeft, leftover) => evalRunner(newLeft, leftover)
    }
  }
}
