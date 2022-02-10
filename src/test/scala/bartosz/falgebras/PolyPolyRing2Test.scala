package bartosz.falgebras

import org.scalatest.funsuite.AnyFunSuite

class PolyPolyRing2Test extends AnyFunSuite {
  test("evaluates expression 1") {
    val p = PolyRing2.Val(List(1, 0, 3, 4, 0))
    val q = PolyRing2.Val(List(1, 0, 0, 4, 1))
    val z = PolyRing2.Val(List(0, 2, 0, 0, 3))
    val w = PolyRing2.Val(List(0, 0, 0, 5, 5))

    val expr = PolyRing2.Mul(
      PolyRing2.Mul(
        p,
        PolyRing2.Add(q, z)
      ),
      w
    )

    print(PolyRing2.eval(expr))
  }
}
