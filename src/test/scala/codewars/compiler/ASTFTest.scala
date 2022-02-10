package codewars.compiler

import bartosz.falgebras.FAlg.Fix
import codewars.compiler.ASTF._
import codewars.compiler.phase1.{AstfSerialization, Function2}
import org.scalatest.flatspec.AnyFlatSpec

class ASTFTest extends AnyFlatSpec {
  "ASTF" should "be serialized to JSON String" in {
    //Prepare
    val expr: Fix[ExprF] = div(
      in(add(varr("x"), varr("y"))),
      num(2)
    )

    val function = Function2(List("x", "y"), expr)

    //Execute
    val actual = AstfSerialization(function)

    //Verify
    val expected = "{\"op\":\"/\",\"a\":{\"op\":\"+\",\"a\":{\"op\":\"arg\",\"n\":0},\"b\":{\"op\":\"arg\",\"n\":1}},\"b\":{\"op\":\"imm\",\"n\":2}}"

    assert(actual === expected)
  }


}
