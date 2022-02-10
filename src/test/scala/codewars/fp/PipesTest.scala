package codewars.fp

import org.scalatest.funsuite.AnyFunSuite

class PipesTest extends AnyFunSuite {
  import codewars.fp.Pipes._

  //Pipe:

  test("Pipe") {
    assert(42.pipe(_ * 2) === 84)
    assert(42.pipe(_.toString) === "42")
  }

  test("Pipe to avoid a local variable") {
    def f(x: Int): Int = x * 3

    // Pipe allows rewriting this:
    val tmp = f(4)
    val a = if (tmp > 10) tmp + 5 else tmp
    assert(a === 17)

    // with this:
    val b = f(4) pipe { case tmp if (tmp > 10) => tmp+5 case tmp => tmp }
    assert(b === 17)
  }

  test("Pipe to chain") {

    val f = (_: Int) * 6
    val g = (_: Int).abs
    val h = (x: Int) => x

    val piped = 1 - 2 - 3 pipe f pipe g pipe h
    val nested = h(g(f(1 - 2 - 3)))
    assert(piped === 24)
    assert(piped === nested)

    // Using the |> alias:
    assert((1 - 2 - 3 |> f |> g |> h) === 24)
  }

  test("Pipe to match") {
    def isPalindrome(i: Int) = i.toString.pipe(s => s == s.reverse)
    assert(isPalindrome(34543))
    assert(34543.toString.pipe(s => s == s.reverse) === (34543.toString match { case s => s == s.reverse }))
  }

  //Tap:

  def log(x: Any): Unit = println(x)

  test("Tap to log") {
    assert(42.tap(x => log(x)) === 42)
    assert(List(1, 2, 3).map(_ * 2).tap(log).map(_ * 2).tap(log) === List(4, 8, 12))
  }

  test("Tap to avoid a local variable") {
    def getA(): Int = 3

    // Pipe allows rewriting this:
    val b1 = {
      val a = getA()
      log(a)
      a
    }

    // with this:
    val b2 = getA().tap(log)
    assert(b2 === 3)
  }

  test("Tap to mutate a state") {
    var a = 1
    val y = 2.tap(x => a += x) + 5
    assert(a === 3)
    assert(y === 7)
  }

  test("Tap each element") {
    // To log:
    assert(List(1, 2, 3).tapEach(log).map(_ * 2) === List(2, 4, 6))

    // To update a mutable state (accumulator):
    var a = 1
    assert(List(1, 2, 3).tapEach(_ => a *= 2).map(_ * a) === List(8, 16, 24))

    // Checking different types of `Iterable`s:
    assert(Map(1 -> 'a', 2 -> 'b', 3 -> 'c').tapEach(log) === Map(1 -> 'a', 2 -> 'b', 3 -> 'c'))
  }

  test("TapEach should preserve the type (if tapEach is called let's say on a List, then the resulting Type should specifically be a List)") {
    assert((0 :: List(1, 2, 3).tapEach(_ => ())) === List(0, 1, 2, 3))
  }
}
