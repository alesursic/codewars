package bartosz.falgebras

//with direct recursion - no catamorphism
object PolyRing2 {
  val MAX_COEF = 5

  sealed trait Ring
  case class Zero() extends Ring
  case class Val(cs: List[Int]) extends Ring
  case class Add(l: Ring, r: Ring) extends Ring
  case class Mul(l: Ring, r: Ring) extends Ring
  case class ScMul(cs: List[Int], sc: (Int, Int)) extends Ring

  def eval(input: Ring): Ring = {
    def evalH: Ring => Ring = {
      //leaf (non-recursive) cases:
      case Zero() => Val(List.fill(MAX_COEF)(0))
      case Val(cs) => Val(cs)
      case ScMul(cs, (coef, pow)) => Val(List.fill(pow)(0) ++ cs.map(coef * _))
      //recursive cases:
      case Add(l, r) => (evalH(l), evalH(r)) match {
        case (Val(p), Val(q)) => Val(p.zip(q).map({ case (a, b) => a + b }))
        case (newL, newR) => evalH(Add(newL, newR)) //repeat
      }
      case Mul(l, r) => (evalH(l), evalH(r)) match {
        case (Val(p), Val(q)) => q.zipWithIndex.map(ScMul(p, _)).fold(Zero())(Add)
        case (newL, newR) => evalH(Mul(newL, newR)) //repeat
      }
    }

    evalH(input) match {
      case Val(cs) => Val(cs)
      case expr => evalH(expr)
    }
  }

  def eval2: Ring => Ring = {
    //leaf (non-recursive) cases:
    case Zero() => Val(List.fill(MAX_COEF)(0))
    case Val(cs) => Val(cs)
    case ScMul(cs, (coef, pow)) => Val(List.fill(pow)(0) ++ cs.map(coef*_))
    //recursive cases:
    case Add(l, r) => (eval2(l), eval2(r)) match {
      case (Val(p), Val(q)) => Val(p.zip(q).map({case (a, b) => a + b}))
    }
    case Mul(l, r) => (eval2(l), eval2(r)) match {
      case (Val(p), Val(q)) => eval2(q.zipWithIndex.map(ScMul(p, _)).fold(Zero())(Add)) //repeat eval2
    }
  }
}
