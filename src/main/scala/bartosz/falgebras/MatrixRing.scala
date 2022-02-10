package bartosz.falgebras

import bartosz.falgebras.Ring._

object MatrixRing {
  case class Matrix(values: ((Int, Int), (Int, Int))) {
    def +(other: Matrix): Matrix = (values, other.values) match {
      case (((x00, x01), (x10, x11)), ((y00, y01), (y10, y11))) => Matrix(
        ((x00+y00, x01+y01), (x10+y10, x11+y11))
      )
    }
    def *(other: Matrix): Matrix = ???
    def unary_!(): Matrix = values match {
      case ((x00, x01), (x10, x11)) => Matrix(((-x00, -x01), (-x10, -x11)))
    }
  }

  val ZerosMatrix = Matrix((0, 0), (0, 0))
  val OnesMatrix = Matrix((1, 1), (1, 1))

  //ring's elements are matrices and result of evaluation of a ring is also a matrix
  def evalM: RingF[Matrix, Matrix] => Matrix = {
    case ZeroF() => ZerosMatrix
    case OneF() => OnesMatrix
    case ValF(m) => m
    case AddF(l, r) => l + r
    case MulF(l, r) => l * r
    case NegF(m) => !m
  }
}
