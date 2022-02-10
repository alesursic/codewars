package bartosz.falgebras

import bartosz.falgebras.FAlg.{ConsF, Fix, Functor, ListF, NilF, listfFunctor}


object PolyEval {
  type IntListF[A] = ListF[Int, A]
  def eval(cs: Fix[IntListF])(x: Int): Int = {
    //2 + 3x + 1x^2 == 1x, (1x + 3)x, (2 + (1x + 3)x)x - 1x too many
    def alg: FAlg.ListF[Int, Int] => Int = {
      case NilF() => 0
      case ConsF(coef, acc) => (acc + coef) * x
    }

    val m: FAlg.Fix[IntListF] => Int = FAlg.cata(alg)
    m(cs) / x
  }

  //for p(x, y) the coefficients are represented in a 2D matrix
  def evalMultiple(css: List[List[Int]])(x: Int, y: Int): Int = {
    def evalSingle(cs: List[Int]): Int = {
      cs.foldRight(0)({
        case (coef, acc) => (acc + coef) * y
      })
    }

    val result = css.foldRight(0)({
      case (coefs, acc) => (acc + evalSingle(coefs)) * x
    })

    result / (y * x)
  }

  //sketch of the solution:
  //first define: Fa -> a
  //then compose: F(Fa) -> a == (Fa -> a) . F(Fa -> a) == (Fa -> a) . F.fmap(Fa -> a)
  def evalMultiple2(css: List[List[Int]])(x: Int, y: Int): Int = {
    //Fa -> a
    def evalAlg(x: Int)(cs: List[Int]): Int = {
      cs.foldRight(0)({
        case (coef, acc) => (acc + coef) * x
      })
    }

    val result = evalAlg(x)(css.map(evalAlg(y))) //(alg . (fmap alg)) css

    result / (x * y)
  }

  def evalMultiple3(css: List[List[Int]])(x: Int, y: Int): Int = {
    //Fa -> a
    def evalAlg: Int => List[Int] => Int = x => cs => {
      cs.foldRight(0)({
        case (coef, acc) => (acc + coef) * x
      })
    }

    val resultFun: List[List[Int]] => Int =
      evalAlg(x) compose (
        _.map(evalAlg(y))
      )

    resultFun(css) / (x * y)
  }

  def eval3dMatrix(css: List[List[List[Int]]])(xs: List[Int]): Int = {
    //Fa -> a
    def evalAlg: Int => List[Int] => Int = x => cs => {
      cs.foldRight(0)({
        case (coef, acc) => (acc + coef) * x
      })
    }

    //_.map(alg compose {..}): List[List[Int]] => List[Int]
    //alg compose {..}: List[Int] => Int
    //_.map(alg compose {_.map(alg compose {..})}): List[List[List[Int]]] => List[Int]

    //Instead of composing functions i.e.: fmapp'ed eval algebras
    //data-structures (functors & fixed-points) should be composed instead
    //An expression tree of fmapp'ed eval algebras should be built and
    //evaluated with catamorphism
    val resultFun: List[List[List[Int]]] => Int = {
      //"The pattern":
      evalAlg(xs(0)) compose (
        _.map(evalAlg(xs(1)) compose (
          _.map(evalAlg(xs(2))) //List[List[Int]] => List[Int]
        ))
      )
    }

    resultFun(css) / xs.sum
  }

  //---------------------------------------------------------------
  //  GENERAL SOLUTION (for any matrix dimension - 1d, 2d, 3d,..)
  //---------------------------------------------------------------

  trait IntTree2F[A]
  case class IntLeaf2F[A](xs: List[Int], x: Int) extends IntTree2F[A]
  case class IntBranch2F[A](children: List[A], x: Int) extends IntTree2F[A]

  def branch2F[A](children: List[Fix[IntTree2F]], x: Int): Fix[IntTree2F] = Fix(IntBranch2F(children, x))
  def leaf2F[A](xs: List[Int], x: Int): Fix[IntTree2F] = Fix(IntLeaf2F(xs, x))

  trait IntTreeF[A]
  case class IntLeafF[A](xs: List[Int]) extends IntTreeF[A]
  case class IntBranchF[A](children: List[A]) extends IntTreeF[A]

  def branchF[A](children: List[Fix[IntTreeF]]): Fix[IntTreeF] = Fix(IntBranchF(children))
  def leafF[A](xs: List[Int]): Fix[IntTreeF] = Fix(IntLeafF(xs))

  val intTreefFunctor = new Functor[IntTree2F] {
    override def fmap[A, B](f: A => B): IntTree2F[A] => IntTree2F[B] = {
      case IntLeaf2F(xs, x) => IntLeaf2F(xs, x)
      case IntBranch2F(children, x) => IntBranch2F(children.map(f), x)
    }
  }

  def evalAnyMatrix(expr: Fix[IntTree2F]): Int = {
    val f: Int => (Int, Int) => Int =
      x => { case (coef, acc) => (acc + coef) * x }

    def evalAlg: IntTree2F[Int] => Int = {
      case IntLeaf2F(cs, x) => cs.foldRight(0)(f(x))
      case IntBranch2F(children, x) => children.foldRight(0)(f(x))
    }

    FAlg.cata(evalAlg)(intTreefFunctor)(expr)
  }

  // Example of a zipped (rewritten) expression where (x, y, z) are points to zip with:
  //    val expr: Fix[IntTree2F] = branch2F(List(
  //      branch2F(List(
  //        leaf2F(List(0, 1, 2), z),
  //        leaf2F(List(1, 0, 2), z),
  //        leaf2F(List(2, 0, 1), z)
  //      ), y),
  //      branch2F(List(
  //        leaf2F(List(0, 2, 0), z),
  //        leaf2F(List(1, 1, 0), z),
  //        leaf2F(List(1, 1, 2), z)
  //      ), y),
  //      branch2F(List(
  //        leaf2F(List(0, 2, 1), z),
  //        leaf2F(List(0, 1, 2), z),
  //        leaf2F(List(1, 1, 2), z)
  //      ), y)
  //    ), x)
  def zip(expr: Fix[IntTreeF], points: List[Int]): Fix[IntTree2F] = (expr, points) match {
    case (Fix(IntLeafF(xs)), h::_) => leaf2F(xs, h)
    case (Fix(IntBranchF(children: List[Fix[IntTreeF]])), h::t) => branch2F(
      children.map(child => zip(child, t)),
      h
    )
  }
}
