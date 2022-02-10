package bartosz.falgebras

import org.scalatest.funsuite.AnyFunSuite

class FAlgTest extends AnyFunSuite {
  //***PEANO NUMBERS***
  test("count peano numbers") {
    val two: FAlg.Fix[FAlg.NatF] = FAlg.Fix(FAlg.SuccF(
      FAlg.Fix(FAlg.SuccF(
        FAlg.Fix(FAlg.ZeroF())
      ))
    ))

    val m: FAlg.Fix[FAlg.NatF] => Int = FAlg.cata(FAlg.peanoNum)

    assert(m(two) === 2)
  }

  test("count list elements") {
    type L[A] = FAlg.ListF[String, A]

    val l: FAlg.Fix[L] = FAlg.Fix(FAlg.ConsF(
      "A",
      FAlg.Fix(FAlg.ConsF(
        "B",
        FAlg.Fix[L](FAlg.NilF())
      ))
    ))

    val m: FAlg.Fix[L] => Int = FAlg.cata(FAlg.listLen)

    assert(m(l) === 2)
  }

  test("count tree nodes") {
    type T[A] = FAlg.TreeF[String, A]

    val t: FAlg.Fix[T] = FAlg.Fix(FAlg.BranchF(
      FAlg.Fix[T](FAlg.LeafF("A")),
      FAlg.Fix[T](FAlg.LeafF("B"))
    ))

    val m: FAlg.Fix[T] => Int = FAlg.cata(FAlg.treeLen)

    assert(m(t) === 3)
  }

  //***LIST***
  //val l2: FAlg.ListF[String, FAlg.ListF[String, _]] = FAlg.ConsF("A", FAlg.NilF) //rewritten
//  test("list count elements [cata]") {
//    val l: FAlg.List[String] = FAlg.Cons("B", FAlg.Cons("A", FAlg.Nil))
//
//    val m: FAlg.List[String] => Int = FAlg.cata(FAlg.lenAlg)
//
//    assert(m(l) === 2)
//  }
//
//  test("list count elements [newCata]") {
//    val l = FAlg.Cons("B", FAlg.Cons("A", FAlg.Nil))
//
//    val m = FAlg.newCata(FAlg.lenAlg)
//
//    assert(m(l) === 2)
//  }



  //***STR-LIST***

//  test("str-list count elements") {
//    val l: FAlg.Fix[FAlg.StrListF] = FAlg.Fix(FAlg.StrConsF(
//      "A",
//      FAlg.Fix(FAlg.StrConsF(
//        "B",
//        FAlg.Fix(FAlg.StrNilF())
//      ))
//    ))
//
//    val m: FAlg.Fix[FAlg.StrListF] => Int = FAlg.cata(FAlg.lenAlg2)
//
//    assert(m(l) === 2)
//  }
//
//  //***STR-TREE**
//
//  test("str-tree count elements") {
//    val t: FAlg.Fix[FAlg.StrTreeF] = FAlg.Fix(FAlg.StrBranchF(
//      FAlg.Fix(FAlg.StrLeafF("A")),
//      FAlg.Fix(FAlg.StrLeafF("B"))
//    ))
//
//    val m = FAlg.cata(FAlg.lenAlg3)
//
//    assert(m(t) === 3)
//  }
}
