package codewars.compiler.phase1

import bartosz.falgebras.FAlg._
import codewars.compiler.ASTF._
import codewars.compiler.dto._
import com.fasterxml.jackson.databind.ObjectMapper

//Uses catamorphism
object AstfSerialization {
  val om = new ObjectMapper()

  def dtoToJson(dto: AstDto): String = om.writeValueAsString(dto)

  def apply(f: CFunction): String = f match {
    case CFunction2(args, expr) => AstSerialization(args.zipWithIndex.toMap).serialize(expr)
  }

  case class AstSerialization(varToIdx: Map[String, Int]) {
    def astToDto(e: Fix[ExprF]): AstDto = {
      def evalAlg: ExprF[AstDto] => AstDto = {
        case NumF(v) => new ImmDto(v)
        case VarF(v) => new ArgDto(varToIdx(v))
        case InF(e) => e
        case DivF(t, f) => new DivDto(t, f)
        case AddF(e, t) => new AddDto(e, t)
      }

      def g: Fix[ExprF] => AstDto = cata(evalAlg)

      g(e)
    }

    def serialize(e: Fix[ExprF]): String = dtoToJson(astToDto(e))
  }
}
