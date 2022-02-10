package bartosz.falgebras

import bartosz.falgebras.FAlg.Fix
import bartosz.falgebras.PolyRing.{AddF, FixRingF, MulF, NegF, OneF, Polynom, RingF, ValF, ZeroF, evalP, fixAddF, fixMulF, fixValF}
import org.scalatest.funsuite.AnyFunSuite

class PolyRingTest extends AnyFunSuite {
  val m: Fix[RingF] => Polynom = FAlg.cata(evalP)

  test("addition") {
    val p: Fix[RingF] = Fix(ValF(List(-1, 0, 4, 0, 0)))
    val q: Fix[RingF] = Fix(OneF())
    val z: Fix[RingF] = Fix(ZeroF())

    val sum: Fix[RingF]= List(p, q).fold(z)({
      case (acc, a) => Fix(AddF(acc, a))
    })

    assert(m(sum) === Polynom(List(0, 1, 5, 1, 1)))
  }

  test("negation") {
    val p: Fix[RingF] = Fix(ValF(List(-1, 0, 4, 0, 0)))
    val neg = Fix(NegF(p))

    assert(m(neg) === Polynom(List(1, 0, -4, 0, 0)))
  }

  //(-1 + 4x^2) * (2 + 3x^2) == (-1 + 4x^2)*2 + (-1 + 4x^2)*3x^2
  // == -2 + 8x^2 - 3x^2 + 12x^4 == -2 + 5x^2 + 12x^4
  // == (-2 + 8x^2) + (-3x^2 + 12x^4) !!
  test("multiplication") {
    val p: Fix[RingF] = Fix(ValF(List(-1, 0, 4, 0, 0)))
    val q: Fix[RingF] = Fix(ValF(List(2, 0, 3, 0, 0)))
    val mul = Fix(MulF(p, q))

    assert(m(mul) === Polynom(List(-2, 0, 5, 0, 12)))
  }

  test("mixed expression") {
    val p = fixValF(List(1, 0, 3, 4, 0))
    val q = fixValF(List(1, 0, 0, 4, 1))
    val z = fixValF(List(0, 2, 0, 0, 3))
    val w = fixValF(List(0, 0, 0, 5, 5))

    //p * (q + z) * w
    val expr: Fix[RingF] = fixMulF(
      fixMulF(
        p,
        fixAddF(q, z)
      ),
      w
    )

    assert(m(expr) === Polynom(List(0, 0, 0, 5, 15)))
  }

  test("mixed expression 2") {
    val p = fixValF(List(1, 0, 3, 4, 0))
    val q = fixValF(List(1, 0, 0, 4, 1))
    val z = fixValF(List(0, 2, 0, 0, 3))
    val w = fixValF(List(0, 0, 0, 5, 5))

    val expr: FixRingF = p * (q :+: z) * w

    assert(m(expr) === Polynom(List(0, 0, 0, 5, 15)))
  }
}
