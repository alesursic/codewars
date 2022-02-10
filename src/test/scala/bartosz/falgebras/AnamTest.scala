package bartosz.falgebras

import bartosz.falgebras.Anam.StreamF
import bartosz.falgebras.FAlg.Fix
import org.scalatest.funsuite.AnyFunSuite

class AnamTest extends AnyFunSuite {
  type IntStreamF[A] = StreamF[Int, A]

  def consF[A](h: Int, t: Fix[IntStreamF]): Fix[IntStreamF] = FAlg.Fix(Anam.ConsStreamF(h, t))
  def nilF[A](): Fix[IntStreamF] = FAlg.Fix(Anam.NilStreamF())

  test("list of squares") {
    val l = List.range(1, 5)

    val actual: Fix[IntStreamF] = Anam.ana(Anam.listOfSquares)(Anam.streamfFunctor)(l)

    assert(actual === consF(1, consF(4, consF(9, consF(16, nilF())))))
  }

  test("list of primes") {
    val l = List.range(2, 10)

    val result: List[Int] = Anam.unfoldr(Anam.era2)(l)

    assert(result === List(2, 3, 5, 7))
  }
}
