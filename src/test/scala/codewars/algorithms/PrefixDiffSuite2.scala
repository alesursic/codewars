package codewars.algorithms

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import codewars.algorithms.PrefixDiff._

class PrefixDiffSuite2 extends AnyFlatSpec with Matchers {
  "Leaf expressions" should "be serialized to String" in {
    assert(diff("5") == "0")
    assert(diff("x") == "1")
    assert(diff("(+ x x)") == "2")
    assert(diff("(- x x)") == "0")
    assert(diff("(* x 2)") == "2")
    assert(diff("(/ x 2)") == "0.5")
    assert(diff("(^ x 2)") == "(* 2 x)")
    assert(diff("(cos x)") == "(* -1 (sin x))")
    assert(diff("(sin x)") == "(cos x)")
    assert(diff("(tan x)") == "(+ 1 (^ (tan x) 2))")
    assert(diff("(exp x)") == "(exp x)")
    assert(diff("(ln x)") == "(/ 1 x)")
  }

  "Nested expressions" should "be serialized to String" in {
    assert(diff("(+ x (+ x x))") == "3")
    assert(diff("(- (+ x x) x)") == "1")
    assert(diff("(* 2 (+ x 2))") == "2")
    assert(diff("(/ 2 (+ 1 x))") == "(/ -2 (^ (+ 1 x) 2))")
    assert(diff("(cos (+ x 1))") == "(* -1 (sin (+ x 1)))")
    assert(diff("(sin (+ x 1))") == "(cos (+ x 1))")
    assert(diff("(sin (* 2 x))") == "(* 2 (cos (* 2 x)))")
    assert(diff("(tan (* 2 x))") == "(* 2 (+ 1 (^ (tan (* 2 x)) 2)))")
    assert(diff("(exp (* 2 x))") == "(* 2 (exp (* 2 x)))")

    val dCos = diff("(cos (* 2 x))")
    assert(dCos == "(* 2 (* -1 (sin (* 2 x))))" || dCos == "(* -2 (sin (* 2 x)))")
  }

  "The only failing nested expression" should "now work" in {
    println(diff("(^ (sin x) 3)"))
  }
}