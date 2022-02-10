package codewars.algorithms

import org.scalatest.funspec.AnyFunSpec

class PrefixDiffSuite extends AnyFunSpec {

  import codewars.algorithms.PrefixDiff._

  describe("Simple tests") {

    val simpleCases = List(
      "5" -> List("0"),
      "x" -> List("1"),
      "(+ x x)" -> List("2"),
      "(- x x)" -> List("0"),
      "(* x 2)" -> List("2"),
      "(/ x 2)" -> List("0.5"),
      "(^ x 2)" -> List("(* 2 x)"),
      "(cos x)" -> List("(* -1 (sin x))"),
      "(sin x)" -> List("(cos x)"),
      "(tan x)" -> List("(+ 1 (^ (tan x) 2))"),
      "(exp x)" -> List("(exp x)"),
      "(ln x)" -> List("(/ 1 x)")
    )
    checkCases(simpleCases)
  }

  describe("Nested expressions") {
    val nestedCases = List(
      "(+ x (+ x x))" -> List("3"),
      "(- (+ x x) x)" -> List("1"),
      "(* 2 (+ x 2))" -> List("2"),
      "(/ 2 (+ 1 x))" -> List("(/ -2 (^ (+ 1 x) 2))"),
      "(cos (+ x 1))" -> List("(* -1 (sin (+ x 1)))"),
      "(sin (+ x 1))" -> List("(cos (+ x 1))"),
      "(sin (* 2 x))" -> List("(* 2 (cos (* 2 x)))"),
      "(tan (* 2 x))" -> List("(* 2 (+ 1 (^ (tan (* 2 x)) 2)))"),
      "(exp (* 2 x))" -> List("(* 2 (exp (* 2 x)))"),
      "(cos (* 2 x))" -> List("(* 2 (* -1 (sin (* 2 x))))", "(* -2 (sin (* 2 x)))")
    )
    checkCases(nestedCases)
  }

  describe("Second derivatives") {
    val secondCases = List(
      "(sin x)" -> List("(* -1 (sin x))"), //(input, expected output)
      "(exp x)" -> List("(exp x)"),
      "(^ x 3)" -> List("(* 3 (* 2 x))", "(* 6 x)")
    )
    for( (expr, possible) <- secondCases ) {
      val rez = possible.mkString(" or ")
      it(s"Second deriv. of $expr should return $rez") {
        val result = diff(diff(expr))
        assert(possible.contains(result))
      }
    }
  }

  def checkCase(expr: String, answers: List[String]): Unit = {
    it(s"should return ${answers.mkString(" or ")} for $expr") {
      val result = diff(expr)
      assert(answers.contains(result))
    }
  }

  def checkCases(cases: List[(String, List[String])]): Unit = {
    cases.foreach { case (expr, answers) => checkCase(expr, answers) }
  }
}