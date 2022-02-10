package rc

import org.scalatest.funsuite.AnyFunSuite
import rc.RcAlgebra._

class RcAlgebraTest extends AnyFunSuite {
  test("can build an effective hierarchy") {
    //Prepare
    val s = Sport("sr:sport:1")
    val c = Category("sr:category:90", s)
    val t1 = Tournament("sr:tournament:1033", Right(c))
    val t0 = Tournament("sr:season:243", Left(t1))
    val m: Match = Match("sr:match:123", t0)

    //Execute
    val actualHierarchy: Hierarchy = toHierarchy(m)

    //Verify
    val expectedHierarchy: Hierarchy = Hierarchy(s, c, t0, m)

    assert(actualHierarchy === expectedHierarchy)
  }

  test("joins chain with another chain when events are contained in cross-groups") {
    //Prepare
    val s = Sport("sr:sport:1")
    val c = Category("sr:category:90", s)
    val t01 = Tournament("sr:tournament:1033", Right(c))
    val t00 = Tournament("sr:season:243", Left(t01))
    val m0: Match = Match("sr:match:123", t00)

    val t11 = Tournament("sr:tournament:1034", Right(c))
    val t10 = Tournament("sr:season:245", Left(t11))
    val m1: Match = Match("sr:match:123", t10)

    val linked = Linked(
      Set(t00, t01),
      Set(t10)
    )

    //Execute
    val newM0 = m0.joinWith(m1)(linked)

    //Verify
    println(newM0)
  }
}
