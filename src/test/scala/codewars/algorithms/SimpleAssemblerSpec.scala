package codewars.algorithms

import codewars.algorithms.SimpleAssembler._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// TODO: replace this example test with your own, this is just here to demonstrate usage.
// See http://www.scalatest.org/at_a_glance for example usages

class SimpleAssemblerSpec extends AnyFlatSpec with Matchers {
  "interpret(List(\"mov a 5\",\"inc a\",\"dec a\",\"dec a\",\"jnz a -1\",\"inc a\"))" should "return Map(\"a\"->1)" in {
    val program = List(
      "mov a 5",
      "inc a",
      "dec a",
      "dec a",
      "jnz a -1",
      "inc a"
    )

    interpret(program) should be (Map("a"->1))
  }

  "interpret(List(\"mov a -10\",\"mov b a\",\"inc a\",\"dec b\",\"jnz a -2\"))" should "return Map(\"a\"->0,\"b\"->-20)" in {
    val program = List(
      "mov a -10",
      "mov b a",
      "inc a",
      "dec b",
      "jnz a -2"
    )

    interpret(program) should be (Map("a"->0,"b"->(-20)))
  }

  "Complex programs" should "work" in {
    val program = List(
      "mov a 1",
      "mov b 1",
      "mov c 0",
      "mov d 26",
      "jnz c 2",
      "jnz 1 5",
      "mov c 7",
      "inc d",
      "dec c",
      "jnz c -2",
      "mov c a",
      "inc a",
      "dec b",
      "jnz b -2",
      "mov b c",
      "dec d",
      "jnz d -6",
      "mov c 18",
      "mov d 11",
      "inc a",
      "dec d",
      "jnz d -2",
      "dec c",
      "jnz c -5"
    )

    interpret(program)
  }
}
