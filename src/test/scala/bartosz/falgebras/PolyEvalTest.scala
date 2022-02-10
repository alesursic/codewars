package bartosz.falgebras

import bartosz.falgebras.FAlg.{ConsF, Fix, ListF, NilF}
import bartosz.falgebras.PolyEval._
import org.scalatest.funsuite.AnyFunSuite

class PolyEvalTest extends AnyFunSuite {
  test("evaluation p(x)") {
    val cs: FAlg.Fix[PolyEval.IntListF] = FAlg.Fix(FAlg.ConsF(
      2,
      FAlg.Fix(FAlg.ConsF(
        3,
        FAlg.Fix(FAlg.ConsF(
          1,
          FAlg.Fix(FAlg.NilF())
        ))
      ))
    ))

    val result: Int = PolyEval.eval(cs)(3)

    assert(result === 20)
  }

  test("evaluation p(x, y) := x^2*y + 5x^3*y^3 + 2y^3 at (5, 2)") {
    val css = List(
      List(0, 0, 0, 2, 0),
      List(0, 0, 0, 0, 0),
      List(0, 1, 0, 0, 0),
      List(0, 0, 0, 5, 0),
      List(0, 0, 0, 0, 0),
    )

    val result = PolyEval.evalMultiple(css)(5, 2)

    assert(result === 5066)
  }

  test("evaluation p(x, y) 2") {
    val css = List(
      List(0, 0, 0, 2, 0),
      List(0, 0, 0, 0, 0),
      List(0, 1, 0, 0, 0),
      List(0, 0, 0, 5, 0),
      List(0, 0, 0, 0, 0),
    )

    val result = PolyEval.evalMultiple2(css)(5, 2)

    assert(result === 5066)
  }

  test("evaluation p(x, y) 3") {
    val css = List(
      List(0, 0, 0, 2, 0),
      List(0, 0, 0, 0, 0),
      List(0, 1, 0, 0, 0),
      List(0, 0, 0, 5, 0),
      List(0, 0, 0, 0, 0),
    )

    val result = PolyEval.evalMultiple3(css)(5, 2)

    assert(result === 5066)
  }

  test("evaluation p(x, y) list-f") {
    type L1d[A] = ListF[Int, A]
    type L2d[A] = ListF[Fix[L1d], A]
    type L3d[A] = ListF[Fix[L2d], A]
    type L4d[A] = ListF[Fix[L3d], A]

    val l2d: Fix[L2d] = Fix(ConsF(
      Fix(ConsF(0, Fix(ConsF(1, Fix(NilF()))))),
      Fix(ConsF(
        Fix(ConsF(1, Fix(ConsF(0, Fix(NilF()))))),
        Fix(NilF())
      ))
    ))

    val l3d: Fix[L3d] = Fix(ConsF(
      l2d,
      Fix(ConsF(
        l2d,
        Fix(NilF())
      ))
    ))
  }

  test("evaluation p(x, y, z)") {
    val matrix = List(
      List(
        List(0, 0, 0, 2, 0),
        List(0, 0, 0, 0, 0),
        List(0, 1, 0, 0, 0),
        List(0, 0, 0, 5, 0),
        List(0, 0, 0, 0, 0),
      ),
      List(
        List(0, 0, 0, 2, 0),
        List(0, 0, 0, 0, 0),
        List(0, 1, 0, 0, 0),
        List(0, 0, 0, 5, 0),
        List(0, 0, 0, 0, 0),
      ),
      List(
        List(0, 0, 0, 2, 0),
        List(0, 0, 0, 0, 0),
        List(0, 1, 0, 0, 0),
        List(0, 0, 0, 5, 0),
        List(0, 0, 0, 0, 0),
      ),
      List(
        List(0, 0, 0, 2, 0),
        List(0, 0, 0, 0, 0),
        List(0, 1, 0, 0, 0),
        List(0, 0, 0, 5, 0),
        List(0, 0, 0, 0, 0),
      ),
      List(
        List(0, 0, 0, 2, 0),
        List(0, 0, 0, 0, 0),
        List(0, 1, 0, 0, 0),
        List(0, 0, 0, 5, 0),
        List(0, 0, 0, 0, 0),
      )
    )

    val result = PolyEval.eval3dMatrix(matrix)(List(5, 2, 7))

    assert(result === 5066)
  }

  //TEST FOR THE GENERAL PURPOSE SOLUTION:

  test("AOT evaluation of 3x3x3 matrix (3d matrix)") {
    val x = 1
    val y = 2
    val z = 3

    val expr: Fix[IntTreeF] = branchF(List(
      branchF(List(
        leafF(List(0, 1, 2)),
        leafF(List(1, 0, 2)),
        leafF(List(2, 0, 1))
      )),
      branchF(List(
        leafF(List(0, 2, 0)),
        leafF(List(1, 1, 0)),
        leafF(List(1, 1, 2))
      )),
      branchF(List(
        leafF(List(0, 2, 1)),
        leafF(List(0, 1, 2)),
        leafF(List(1, 1, 2))
      ))
    ))

    val result = evalAnyMatrix(zip(expr, List(x, y, z)))

    print(result / (x * y * z))
  }
}
