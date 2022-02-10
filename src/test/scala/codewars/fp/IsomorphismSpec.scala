package codewars.fp

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IsomorphismSpec extends AnyFlatSpec with Matchers {
  import codewars.fp.Isomorphism._

  val bISO: ISO[Boolean, Boolean] = (!_, !_)
  def lrl[A, B](iso: ISO[A, B]): A => A =
    a => substR(iso)(substL(iso)(a))

  "substL" should "get A => B" in {
    substL(bISO)(true) shouldBe false
    substL(bISO)(false) shouldBe true
    substL(isoBool)(false) shouldBe false
    substL(isoBool)(true) shouldBe true
  }

  "substR" should "get B => A" in {
    substR(bISO)(true) shouldBe false
    substR(bISO)(false) shouldBe true
    substR(isoBool)(false) shouldBe false
    substR(isoBool)(true) shouldBe true
  }

  "combining isomorphisms" should "work with isoEU" in {
    val input: Either[List[Unit], Unit] = Right(())
    val output: Either[List[Unit], Nothing] = substL(isoEU)(input)
    output.isLeft shouldBe true

    for (i <- 1 to 10) {
      val lst = List.fill(i)(())
      lrl(isoEU)(Left(lst)) shouldBe Left(lst)
    }
  }

  //--------------------------------------------------------------------------------------

  //Either[..]

  //IF input = Right(()) WILL THIS HOLD g(f(input)) == input ?
  "combining isomorphisms" should "work with isoEU 2" in {
    val input: Either[List[Unit], Unit] = Right(())
    val output: Either[List[Unit], Nothing] = substL(isoEU)(input)

    output.isLeft shouldBe true
    val newInput = substR(isoEU)(output)

    newInput shouldBe input
  }

  "combining isomorphisms" should "work with isoEU on empty List 3" in {
    //1.
    val input: Either[List[Unit], Unit] = Left(List())
    val output: Either[List[Unit], Nothing] = substL(isoEU)(input)

    output.isLeft shouldBe true
    val newInput = substR(isoEU)(output)

    newInput shouldBe input

    //2.
    substR(isoEU)(Left(List())) shouldBe Left(List())
  }

  "combining isomorphisms" should "work with isoEU 4" in {
    val input: Either[List[Unit], Unit] = Right(())
    val output: Either[List[Unit], Nothing] = substL(isoEU)(input)

    output.isLeft shouldBe true
    val newInput = substR(isoEU)(output)

    newInput shouldBe input

    //2.
    substR(isoEU)(Left(List())) shouldBe List()
  }

  //Option[Boolean]

  "combining isomorphisms" should "work for Opt[Bool] both ways" in {
    //Prepare
    val boolOptIso: ISO[Option[Boolean], Option[Boolean]] = {
      val fOpt: Option[Boolean] => Option[Boolean] = {
        case None => Some(true)
        case Some(true) => None
        case Some(false) => Some(false)
      }

      (fOpt, fOpt) //fOpt == gOpt
    }

    //"Execute"/Compose
    val boolIso: ISO[Boolean, Boolean] = isoUnOption(boolOptIso)

    //Verify
    println(substL(boolIso)(false))
    println(substL(boolIso)(true))
    println(substR(boolIso)(false))
    println(substR(boolIso)(true))

    val id = lrl(boolIso)

    id(false) shouldBe false
    id(true) shouldBe true
  }

  "combining isomorphisms" should "work for Opt[Bool] both ways 2" in {
    //Prepare
    val boolOptIso: ISO[Option[Boolean], Option[Boolean]] = {
      val fOpt: Option[Boolean] => Option[Boolean] = {
        case None => None
        case Some(true) => Some(false)
        case Some(false) => Some(true)
      }

      (fOpt, fOpt) //fOpt == gOpt
    }

    //"Execute"/Compose
    val boolIso: ISO[Boolean, Boolean] = isoUnOption(boolOptIso)

    //Verify
    println(substL(boolIso)(false))
    println(substL(boolIso)(true)) //!
    println(substR(boolIso)(false))
    println(substR(boolIso)(true))

    val id = lrl(boolIso)

    id(false) shouldBe false
    id(true) shouldBe true
  }
}