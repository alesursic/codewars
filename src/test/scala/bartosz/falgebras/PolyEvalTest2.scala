package bartosz.falgebras

import org.scalatest.funsuite.AnyFunSuite

class PolyEvalTest2 extends AnyFunSuite {
  trait RoseTree
  case class Leaf(x: Int) extends RoseTree
  case class Branch(children: List[RoseTree], point: Int) extends RoseTree

  test("test 1") {
    val y = 1
    val x = 2

    val lf0: RoseTree = Branch(
      List(
        Branch(List(Leaf(0), Leaf(1), Leaf(2)), y),
        Branch(List(Leaf(2), Leaf(1), Leaf(0)), y)
      ),
      x
    )
  }
}
