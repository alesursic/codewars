package codewars.compiler.phase1

import codewars.compiler.AST._
import codewars.compiler.dto._
import com.fasterxml.jackson.databind.ObjectMapper

//Uses direct/explicit recursion
object AstSerialization {
  val om = new ObjectMapper()

  def dtoToJson(dto: AstDto): String = om.writeValueAsString(dto)

  def apply(varToIdx: Map[String, Int]) = AstSerialization(varToIdx)
  def apply(f: CFunction): String = f match {
    case CFunction1(args, expr) => AstSerialization(args.zipWithIndex.toMap).serialize(expr)
  }

  case class AstSerialization(varToIdx: Map[String, Int]) {
    def astToDto(e: Expr): AstDto = e match {
      case Num(v) => new ImmDto(v)
      case Var(v) => new ArgDto(varToIdx(v))
      case In(e) => astToDto(e)
      case Div(t, f) => new DivDto(astToDto(t), astToDto(f))
      case Add(e, t) => new AddDto(astToDto(e), astToDto(t))
    }

    def serialize(e: Expr): String = dtoToJson(astToDto(e))
  }
}
