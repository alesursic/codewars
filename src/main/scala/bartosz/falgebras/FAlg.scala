package bartosz.falgebras

object FAlg {
  //!!!CATAMORPHISM!!!
  //Used by peano-num, str-list and str-tree initial algebras
  def cata[A, F[_]](alg: F[A] => A)(implicit F: Functor[F]): Fix[F] => A = fix => {
    val unFixed: F[Fix[F]] = unFix(fix)
    val mapped: F[A] = F.fmap(cata(alg))(unFixed) //implicit functor used here
    alg(mapped)
  }

  //***FIXED-POINT***
  final case class Fix[F[_]](unFix: F[Fix[F]])
  def unFix[F[_]](fix: Fix[F]): F[Fix[F]] = fix.unFix
  object Fix { def apply[F[_]](f: F[Fix[F]]): Fix[F] = new Fix(f) }

  //***FUNCTOR ABSTRACTION (parameterizes over a parameterized type)***
  //kind: (* -> *) -> * [order-2]
  //F[_] is a first-order type. F[_] is also a type constructor, i.e.
  //(function that takes a type and builds a new type)
  //Functor[F[_]] is an abstraction over a first-order type: a second-order type
  //(function that takes a type which takes a type to build a new type and then builds a new type)
  trait Functor[F[_]] { def fmap[A, B](f: A => B): F[A] => F[B] }

  //***FUNCTOR DEFINITIONS (Fa)***

  trait NatF[A]
  case class ZeroF[A]() extends NatF[A]
  case class SuccF[A](a: A) extends NatF[A]

  implicit val natfFunctor = new Functor[NatF] {
    override def fmap[A, B](f: A => B): NatF[A] => NatF[B] = {
      case ZeroF() => ZeroF()
      case SuccF(a) => SuccF(f(a))
    }
  }

  sealed trait ListF[E, A] { def fmap[B](f: A => B): ListF[E, B] = ??? }
  case class NilF[E, A]() extends ListF[E, A]
  case class ConsF[E, A](h: E, t: A) extends ListF[E, A]

  //# - type projection to an anonymous (new) type
  implicit def listfFunctor[E] = new Functor[({ type L[A] = ListF[E, A] }) # L] {
    override def fmap[A, B](f: A => B): ListF[E, A] => ListF[E, B] = {
      case NilF() => NilF()
      case ConsF(h, t) => ConsF(h, f(t))
    }
  }

  sealed trait TreeF[E, A]
  case class LeafF[E, A](x: E) extends TreeF[E, A]
  case class BranchF[E, A](l: A, r: A) extends TreeF[E, A]

  implicit def treefFunctor[E] = new Functor[({type T[A] = TreeF[E, A]}) # T] {
    override def fmap[A, B](f: A => B): TreeF[E, A] => TreeF[E, B] = {
      case LeafF(x) => LeafF(x)
      case BranchF(l, r) => BranchF(f(l), f(r))
    }
  }

  //***EVALUATION FUNCTIONS (Fa -> a)***

  def peanoNum: NatF[Int] => Int = {
    case ZeroF() => 0
    case SuccF(n) => 1 + n
  }

  def listLen[E]: ListF[E, Int] => Int = {
    case NilF() => 0
    case ConsF(_, n) => n + 1
  }

  def treeLen[E]: TreeF[E, Int] => Int = {
    case LeafF(_) => 1
    case BranchF(l, r) => 1 + l + r
  }


  //***!!!EXTRA!!!***

  sealed trait List[+E]
  case object Nil extends List[Nothing]
  case class Cons[E](h: E, t: List[E]) extends List[E]

  //***STRING-LIST***
  sealed trait StrListF[A]
  case class StrNilF[A]() extends StrListF[A]
  case class StrConsF[A](h: String, t: A) extends StrListF[A]

  implicit val strListfFunctor = new Functor[StrListF] {
    override def fmap[A, B](f: A => B): StrListF[A] => StrListF[B] = {
      case StrNilF() => StrNilF()
      case StrConsF(h, t) => StrConsF(h, f(t))
    }
  }

  //***STRING-TREE***
  sealed trait StrTreeF[A]
  case class StrLeafF[A](x: String) extends StrTreeF[A]
  case class StrBranchF[A](l: A, r: A) extends StrTreeF[A]

  implicit val strTreefFunctor = new Functor[StrTreeF] {
    override def fmap[A, B](f: A => B): StrTreeF[A] => StrTreeF[B] = {
      case StrLeafF(x) => StrLeafF(x)
      case StrBranchF(l, r) => StrBranchF(f(l), f(r))
    }
  }

  def lenAlg2: StrListF[Int] => Int = {
    case StrNilF() => 0
    case StrConsF(_, n) => n + 1
  }

  def lenAlg3: StrTreeF[Int] => Int = {
    case StrLeafF(_) => 1
    case StrBranchF(l, r) => 1 + l + r
  }

  //get rid of this function with Fix
  def toListF[E]: List[E] => ListF[E, List[E]] = {
    case Nil => NilF()
    case Cons(h, t) => ConsF(h, t)
  }

  //functor as a part of implementation
  def cata2[E, A](alg: ListF[E, A] => A): List[E] => A = {
    case Nil => alg(NilF())
    case Cons(h, t) =>
      val newT: A = cata2(alg)(t) //applies catamorphism on tail (the recursion)
      alg(ConsF(h, newT)) //applies algebra on the result of recursion
  }

  //"external" functor used
  def newCata[E, A](alg: ListF[E, A] => A): List[E] => A =
    alg compose { toListF(_).fmap(newCata(alg)) }
//  def cata[A, F[_]](alg: F[A] => A)(implicit F: Functor[F]): Fix[F] => A = {
//    alg.compose(F.fmap(cata(alg)) _ compose unFix)
//  }
}
