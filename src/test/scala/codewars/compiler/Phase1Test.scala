package codewars.compiler

import codewars.compiler.ASTF.{add, div, in, mul, num, varr}
import codewars.compiler.phase1.{AstfDeserialization, Function2}
import org.scalatest.flatspec.AnyFlatSpec

class Phase1Test extends AnyFlatSpec {
  "Program 1" should "be deserialized into AST" in {
    val program = "[ a b ] a*a + b*b"

    val function = AstfDeserialization(program)

    val expectedAst = add(
      mul(varr("a"), varr("a")),
      mul(varr("b"), varr("b")),
    )
    val expectedFunction = Function2(List("a", "b"), expectedAst)

    assert(expectedFunction === function)
  }

  "Program 2" should "be deserialized into AST" in {
    val program = "[ a0 a1 b0 b1 ] a0+a1 + b0*b1"

    val function = AstfDeserialization(program)

    val expectedAst = add(
      add(varr("a0"), varr("a1")),
      mul(varr("b0"), varr("b1"))
    )
    val expectedFunction = Function2(List("a0", "a1", "b0", "b1"), expectedAst)

    assert(expectedFunction === function)
  }

  "Program 3" should "be deserialized into AST" in {
    val program = "[ first second ] (first + second) / 2"

    val function = AstfDeserialization(program)

    val expectedAst = div(
      in(add(varr("first"), varr("second"))),
      num(2)
    )
    val expectedFunction = Function2(List("first", "second"), expectedAst)

    assert(expectedFunction === function)
  }
}
