package codewars.fp

import codewars.fp.Cata._
import org.scalatest.funsuite.AnyFunSuite

class CataTest extends AnyFunSuite {
  test("ListF count & concat") {
    val l: Fix[ListF] = Fix(ConsF("A", Fix(ConsF("B", Fix(NilF())))))

    assert(countListF(l) === 2)
    assert(concatListF(l) == "AB")

    val l2 = FixConsF("A", FixConsF("B", FixConsF("C", FixNilF())))

    assert(countListF(l2) === 3)
    assert(concatListF(l2) === "ABC")
  }

  test("TreeF count & concat") {
    val t: Fix[TreeF] = FixNodeF(FixNodeF(FixLeafF("A"), FixLeafF("B")), FixLeafF("C"))

    assert(countTreeF(t) === 5)
    assert(concatTreeF(t) === "ABC")
  }

  test("Fixed-point map") {
    val treeMap0: Map[String, Map[String, String]] = Map(
      "A" -> Map(
        "B" -> null,
        "C" -> null
      )
    )

    val treeMap1: Map[String, Map[String, Map[String, String]]] = Map(
      "A" -> Map(
        "B" -> Map(
          "D" -> null,
          "E" -> null
        ),
        "C" -> null
      )
    )

    //how to fix map into Map[String, Fix[Map]]
//    val f0: FixBi[Map] = FixBi(Map[String, String]("D" -> null))
  }
//  test("OptionF") {
//    val x0: Fix[OptionF] = Fix(NoneF())
//    val x1: Fix[OptionF] = Fix(SomeF(x0))
//    val x2: OptionF[Fix[OptionF]] = SomeF(x1)
//    val x3: Fix[OptionF] = Fix(SomeF(x1)) //value of x3 is: "Fix(SomeF(Fix(SomeF(Fix(NoneF())))))"
//
//    val x0New: OptionF[Fix[OptionF]] = x0.unfix
//    val x1New: OptionF[Fix[OptionF]] = x1.unfix
//  }
//
//  test("Option(F) vs. Fix-ed OptionF") {
//    val x0: OptionF[OptionF[OptionF[OptionF[_]]]] = SomeF(SomeF(SomeF(NoneF())))
//    val x1: Fix[OptionF] = Fix(SomeF(Fix(SomeF(Fix(SomeF(Fix(NoneF()))))))) //x0 rewritten
//  }
//
//  test("ListF") {
//    val l0: Fix[ListF] = Fix(NilF())
//    val l1: ListF[Fix[ListF]] = ConsF(1, l0)
//    val l2: Fix[ListF] = Fix(l1)
//
//    println(l1.getClass)
//    println(l2.getClass)
//  }
}
