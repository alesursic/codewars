package codewars.compiler

import org.scalatest.flatspec.AnyFlatSpec
import codewars.compiler.ASTF._
import codewars.compiler.phase2.Simplify.simplifyExpr


class Phase2Test extends AnyFlatSpec {
  //[ x ] x + 2*5
  "AST 1" should "be simplified into a new AST" in {
    val input = add(varr("x"), mul(num(2), num(5)))

    val output = simplifyExpr(input)

    val expected = add(varr("x"), num(10))

    assert(output === expected)
  }

  //"[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1 + 3 + 2*2)"
  "AST 2" should "be simplified into a new AST" in {
    val input = div(
      sub(
        add(
          mul(mul(num(2), num(3)), varr("x")),
          mul(num(5), varr("y"))
        ),
        mul(num(3), varr("z"))
      ),
      add(
        add(num(1), num(3)),
        mul(num(2), num(2))
      )
    )

    val output = simplifyExpr(input)

    val expected = div(
      sub(
        add(
          mul(num(6), varr("x")),
          mul(num(5), varr("y"))
        ),
        mul(num(3), varr("z"))
      ),
      num(8)
    )

    assert(output === expected)
  }
}
