package codewars.fp

object Cata {
  type FAlgebra[F[_], A] = F[A] => A
  trait Functor[F[_]] { def fmap[A, B](f: A => B): F[A] => F[B] }
  final case class Fix[F[_]](unfix: F[Fix[F]]) //Fix f := Fix { f Fix f }

  //> newtype Fix s a = In { out :: s a (Fix s a) }
  final case class FixBi[F[_, _]](unfix: F[_, FixBi[F]])

  /**
   * Catamorphism (fold over a general recursive data structure)
   *
   * Recursive function on a recursive data-type is divided into:
   *  1. A functor that tells how to apply a recursive function on the recursive data-type
   *  2. A non-recursive function that maps values into desired values
   *      (e.g. single step function describing the business logic)
   *
   * On top of that the recursive data-type is rewritten into "fixed" recursive data-type.
   * The two types are isomorphic. Fixing a recursive data-type is modeling a call stack.
   *
   *
   * @param alg - maps values into desired values (tells how "extract" a final value from a functor)
   * @param fix - "fixed" recursive data-type
   * @param F - implementation of a functor "instructing" how to map (or distribute) cata on a recursive data-type
   *
   * @tparam F - a functor data-type
   * @tparam A - resulting non-recursive data-type
   *
   * @return a "folded" value
   */
  def cata[F[_], A](alg: F[A] => A)(fix: Fix[F])(implicit F: Functor[F]): A = {
    val fr: F[Fix[F]] = fix.unfix
    val fmapCata: F[Fix[F]] => F[A] = F.fmap(cata(alg)) //takes a function and produces a function that takes a functor
    val fa: F[A] = fmapCata(fr)

    alg(fa) //alg compose F.fmap(cata(alg)) compose fix.unfix
  }

  //Let's use the catamorphism

  //***LIST***

  trait List1[A]
  case class Nil1[A]() extends List1[A]
  case class Cons1[A](h: A, t: Cons1[A]) extends List1[A]

  //"business logic" functions:

  def count[A](l: List1[A]): Int = l match {
    case Nil1() => 0
    case Cons1(_, t) => 1 + count(t)
  }

  def multiply(l: List1[Int]): Int = l match {
    case Nil1() => 1
    case Cons1(h, t) => h * multiply(t)
  }

  def concat(l: List1[String]): String = l match {
    case Nil1() => ""
    case Cons1(h, t) => h ++ concat(t)
  }

  //expressing functions above with catamorphism

  trait ListF[A]
  case class NilF[A]() extends ListF[A]
  case class ConsF[A](h: String, t: A) extends ListF[A]

  //helpers
  def FixConsF(h: String, t: Fix[ListF]): Fix[ListF] = Fix(ConsF(h, t))
  def FixNilF(): Fix[ListF] = Fix(NilF())

  implicit val listFunctor = new Functor[ListF] {
    override def fmap[A, B](recFun: A => B): ListF[A] => ListF[B] = {
      case NilF() => NilF()
      case ConsF(h, t) => ConsF(h, recFun(t))
    }
  }

  def countListF(l: Fix[ListF]): Int = {
    val alg: FAlgebra[ListF, Int] = {
      case NilF() => 0
      case ConsF(_, t) => 1 + t
    }

    cata(alg)(l)
  }

  def concatListF(l: Fix[ListF]): String = {
    val alg: FAlgebra[ListF, String] = {
      case NilF() => ""
      case ConsF(h, t) => h ++ t
    }

    cata(alg)(l)
  }

  //***TREE***

  trait TreeF[A]
  case class LeafF[A](x: String) extends TreeF[A]
  case class NodeF[A](l: A, r: A) extends TreeF[A]

  //helpers
  def FixLeafF(x: String): Fix[TreeF] = Fix(LeafF(x))
  def FixNodeF(l: Fix[TreeF], r: Fix[TreeF]): Fix[TreeF] = Fix(NodeF(l, r))

  implicit val treeFunctor = new Functor[TreeF] {
    override def fmap[A, B](recFun: A => B): TreeF[A] => TreeF[B] = {
      case LeafF(x) => LeafF(x)
      case NodeF(l, r) => NodeF(recFun(l), recFun(r))
    }
  }

  def countTreeF(t: Fix[TreeF]): Int = {
    val alg: FAlgebra[TreeF, Int] = {
      case LeafF(_) => 1
      case NodeF(l, r) => l + r + 1
    }

    cata(alg)(t)
  }

  def concatTreeF(l: Fix[TreeF]): String = {
    val alg: FAlgebra[TreeF, String] = {
      case LeafF(x) => x
      case NodeF(l, r) => l ++ r
    }

    cata(alg)(l)
  }

//  type Fold[A, B] = List1[A] => B => ((A, B) => B) => B
//  def newFold[A, B]: Fold[A, B] = fold
//  def fold[A, B](l: List1[A])(e: B)(f: (A, B) => B): B = l match {
//    case Nil1 => e
//    case Cons1(h, t) => f(h, fold(t)(e, f))
//  }

//  def newFold2[F[_], A](implicit F: Functor[F]): FoldF[F, A] = (l: ListF[F]) => e => f => {
    //    val alg: F[A] => A = null //todo: build from e, f
    //    val fix: Fix[F] = null //todo: build from l
    //
    //    cata(alg)(fix)
    //  }

  //  //1. adt
//  trait OptionF[A]
//  case class NoneF[A]() extends OptionF[A]
//  case class SomeF[A](a: A) extends OptionF[A]
//
//  //2. adt
//  sealed trait ListF[A]
//  case class NilF[A]() extends ListF[A]
//  case class ConsF[A](h: Int, t: A) extends ListF[A]
//


//  // cata :: R => A
//  def cata2[F[_], R, A](alg: F[A] => A, out: R => F[R])(r: R)(implicit F: Functor[F]): A = {
//    val fr: F[R] = out(r)
//    val cata2Mapper: R => A = r => cata2(alg, out)(r) //cata2(alg, out)
//    val fmapCata: F[R] => F[A] = F.fmap(cata2Mapper)
//    val fa: F[A] = fmapCata(fr)
//
//    alg(fa)
//  }
//
//  def cata3[F[_], R, A](alg: F[A] => A, out: R => F[R])(implicit F: Functor[F]): R => A = r => {
//    val fmapCata: F[R] => F[A] = F.fmap(cata3(alg, out))
//    val fr: F[R] = out(r)
//
//    alg(fmapCata(fr))
//  }
//
//  def cata4[F[_], R, A](alg: F[A] => A, out: R => F[R])(implicit F: Functor[F]): R => A = {
//    val fmapCata: F[R] => F[A] = F.fmap(cata4(alg, out))
//
//    alg compose fmapCata compose out
//  }
//
//  def cata5[F[_], R, A](alg: F[A] => A, out: R => F[R])(implicit F: Functor[F]): R => A =
//    alg compose F.fmap(cata5(alg, out)) compose out
}
