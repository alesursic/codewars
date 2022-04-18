package codewars.compiler

import codewars.compiler.AST._
import codewars.compiler.dto._
import codewars.compiler.phase1.{AstSerialization, CFunction1}
import org.scalatest.flatspec.AnyFlatSpec

class ASTTest extends AnyFlatSpec {
  "AST" should "be serialized to JSON String" in {
    //Prepare
    val expr = Div(
      In(Add(Var("x"), Var("y"))),
      Num(2)
    )

    val function  = CFunction1(List("x", "y"), expr)

    //Execute
    val actual = AstSerialization(function)

    //Verify
    val expected = "{\"op\":\"/\",\"a\":{\"op\":\"+\",\"a\":{\"op\":\"arg\",\"n\":0},\"b\":{\"op\":\"arg\",\"n\":1}},\"b\":{\"op\":\"imm\",\"n\":2}}"

    assert(actual === expected)
  }

  //Not really needed tests (just to show how to manually build expression trees):

  "AST" should "be folded to JSON objects" in {
    val expr0 = Add(
      Mul(Var("a"), Var("a")),
      Mul(Var("b"), Var("b"))
    )

    println(expr0)

    val expr1 = Div(
      In(Add(Var("first"), Var("second"))),
      Num(2)
    )

    println(expr1)
  }

  "JSON objects" should "be serialized to JSON String" in {
    val dto: AstDto = new DivDto(
      new AddDto(new ArgDto(0), new ArgDto(1)),
      new ImmDto(2)
    )

    println(AstSerialization.dtoToJson(dto))
  }


}
