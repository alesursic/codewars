package codewars.fp

import codewars.fp.Scott.Scott1.{SC0, SC1, SC2, SCS}
import codewars.fp.Scott.{Scott1, _}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScottSpec extends AnyFlatSpec with Matchers {
  "The Option type" can "be cast to scala Option" in {
    //B == Option[Int]
    toOption(new SOption[Int] {
      def apply[B] = (z, _) => z
    }) should be(None)

    toOption(new SOption[Int] {
      def apply[B] = (_, f) => f(4)
    }) should be(Some(4))
  }

  it can "be cast from scala Option" in {
    fromOption[Int](None)[Int](0, _ + 1) should be(0)
    fromOption(Some(4))[Int](0, _ + 1) should be(5)
  }

  "The List type" can "be cast to scala List" in {
    toList(nil[Int]) should be(List())

    toList(new SList[Int] {
      override def apply[B] = (_, f) => f(1, new SList[Int] {
        override def apply[B] = (_, g) => g(2, nil[Int])
      })
    }) should be(List(1, 2))
  }

  it can "be cast from scala List" in {
    fromList[Int](List())[Int](0, reduce) should be(0)
    fromList[Int](List(1, 2, 3))[Int](0, reduce) should be(321)
  }

  "The Either type" can "be cast to scala Either" in {
    toEither(new SEither[Int, String] {
      override def apply[C] = (left, _) => left(3)
    }) should be(Left(3))

    toEither(new SEither[Int, String] {
      override def apply[C] = (_, right) => right("hello")
    }) should be(Right("hello"))
  }

  it can "be cast from scala Either" in {
    fromEither[Int, String](Left(3))[String](_.toString, identity) should be("3")
    fromEither[Int, String](Right("hello"))[String](_.toString, identity) should be("hello")
  }

  "The tuple type" can "be cast to (,)" in {
    toTuple(new STuple[Int, String] {
      override def apply[C] = f => f(2, "hi")
    }) should be((2, "hi"))
  }

  it can "be cast from (,)" in {
    fromTuple((2, "hi"))[List[String]](List.fill(_)(_)) should be(List("hi", "hi"))
    fromTuple((3, 6))[Int](_ * _) should be(18)
  }

  def reduce(i: Int, is: SList[Int]): Int =
    i + 10 * is[Int](0, reduce)

  "partition" should "partition eithers into tuple of lists" in {
    val input = fromList(List(fromEither(Left(1)), fromEither(Right(2)), fromEither(Left(3))))

    val output = partition(input)

//    val expected = fromTuple(fromList(List(1, 3)), fromList(List(2)))
//    output shouldBe expected

    val (left, right) = toTuple(output)

    println(toList(left))
    println(toList(right))
  }

  //*******************************************Temp*****************************************

  val sc0: SC0 = new SC0 {
    override def apply(f: () => String) = f()
  }

  val sc1 = new SC1 {
    override def apply(f: (String) => String) = f("Ales")
  }

  val sc2 = new SC2 {
    //data is stored as parameters to a function which will be called when it's given
    //data is accessed by a function f when it's called by apply(f)
    override def apply(f: (String, String) => String) = f("Ales", "Maja")
  }

  "encoding" should "encode" in {
    //Execute (give the function so that the Encodings above can call it)
    val x0 = sc0(() => "Hello")
    val x1 = sc1(identity)
    val x2 = sc2(_ ++ _)

    //Verify
    x0 shouldBe "Hello"
    x1 shouldBe "Ales"
    x2 shouldBe "AlesMaja"
  }

  "branching" should "select the right scott-encoding" in {
    val f0 = () => "Hello"
    val f1: String => String = identity
    val f2: (String, String) => String = _ ++ _

    val scs = Scott1.SCS(sc0, sc1, sc2)

    /*
     * Scott's encoding (constructor) is the ability to store arguments for
     * constructor with different arity. Arguments are then passed to a given
     * function for that same arity
     */

    //branching of scott-encodings (ctors) based on the given function's arity
    scs(f0) shouldBe "Hello"
    scs(f1) shouldBe "Ales"
    scs(f2) shouldBe "AlesMaja"
  }
}
