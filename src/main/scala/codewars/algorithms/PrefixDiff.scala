package codewars.algorithms

//complexity = 2 kyu
object PrefixDiff {
  //Constants
  val SPACE = " "
  val BEGIN = "("
  val END = ")"
  val PLUS = "+"
  val MINUS = "-"
  val MULTIPLY = "*"
  val DIVIDE = "/"
  val POW = "^"
  val SIN = "sin"
  val COS = "cos"
  val TAN = "tan"
  val EXP = "exp"
  val LN = "ln"

  //Expressions [AST]
  trait Expr {
  }

  case object NilExpr extends Expr

  //Values:

  object Const { def apply(value: String): Const = Const(value.toFloat) }

  case class Const(value: Double) extends Expr {
    override def equals(obj: Any) = obj match {
      case Const(other) => value == other
      case _ => false
    }

    override def toString = {
      if (value == value.round) value.round.toString else value.toString
    }
  }

  case class Var(symbol: String) extends Expr {
    override def equals(obj: Any) = obj match {
      case Var(other) => symbol == other
      case _ => false
    }

    override def toString = symbol
  }

  //Binary operators:

  class BinOp(op: String, left: Expr, right: Expr) extends Expr {
    override def toString = String.format("(%s %s %s)", op, left, right)
  }

  //commutative
  case class Sum(left: Expr, right: Expr) extends BinOp(PLUS, left, right) {
    override def equals(obj: Any) = obj match {
      case Sum(otherLeft, otherRight) => left == otherLeft && right == otherRight
      case _ => false
    }
  }

  case class Subtract(left: Expr, right: Expr) extends BinOp(MINUS, left, right) {
    override def equals(obj: Any) = obj match {
      case Subtract(otherLeft, otherRight) => left == otherLeft && right == otherRight
      case _ => false
    }
  }

  //commutative
  case class Product(left: Expr, right: Expr) extends BinOp(MULTIPLY, left, right) {
    override def equals(obj: Any) = obj match {
      case Product(otherLeft, otherRight) => left == otherLeft && right == otherRight
      case _ => false
    }
  }

  case class Divide(left: Expr, right: Expr) extends BinOp(DIVIDE, left, right) {
    override def equals(obj: Any) = obj match {
      case Divide(otherLeft, otherRight) => left == otherLeft && right == otherRight
      case _ => false
    }
  }

  case class Pow(base: Expr, exponent: Expr) extends BinOp(POW, base, exponent) {
    override def equals(obj: Any) = obj match {
      case Pow(otherBase, otherExponent) => base == otherBase && exponent == otherExponent
      case _ => false
    }
  }

  //Unary operators:

  class UnOp(op: String, expr: Expr) extends Expr {
    override def toString = String.format("(%s %s)", op, expr)
  }

  case class Sin(expr: Expr) extends UnOp(SIN, expr) {
    override def equals(obj: Any) = obj match {
      case Sin(other) => expr == other
      case _ => false
    }
  }

  case class Cos(expr: Expr) extends UnOp(COS, expr) {
    override def equals(obj: Any) = obj match {
      case Cos(other) => expr == other
      case _ => false
    }
  }

  case class Tan(expr: Expr) extends UnOp(TAN, expr) {
    override def equals(obj: Any) = obj match {
      case Tan(other) => expr == other
      case _ => false
    }
  }

  case class Exp(expr: Expr) extends UnOp(EXP, expr) {
    override def equals(obj: Any) = obj match {
      case Exp(other) => expr == other
      case _ => false
    }
  }

  case class Ln(expr: Expr) extends UnOp(LN, expr) {
    override def equals(obj: Any) = obj match {
      case Ln(other) => expr == other
      case _ => false
    }
  }

  //Expression parsers
  abstract class Parser(tokens: List[String]) {
    def eval(): (Expr, Parser)

    //Additional methods:
    def map(f: (Expr, Parser) => (Expr, Parser)): (Expr, Parser) = {
      eval() match {
        case (expr, parser) => f(expr, parser)
      }
    }

    def map2(f: (Expr, Expr, Parser) => (Expr, Parser)): (Expr, Parser) = map {
      (left, parser) => parser map {
        (right, nextParser) => f(left, right, nextParser)
      }
    }

    def skip() = ExprParser(tokens.tail)
    def isEmpty() = tokens.isEmpty
  }

  case class ExprParser(tokens: List[String]) extends Parser(tokens) {
    override def eval() = tokens match {
      case head :: tail =>
        if (isAlphaNumeric(head))
          ValParser(head :: tail).eval()
        else
          OpParser(tail) map { (expr, parser) => (expr, parser.skip()) }

      case Nil => (NilExpr, this)
    }
  }

  case class OpParser(tokens: List[String]) extends Parser(tokens) {
    override def eval() = tokens match {
      case head :: tail => head match {
        case PLUS => tail ~> Sum
        case MINUS => tail ~> Subtract
        case MULTIPLY => tail ~> Product
        case DIVIDE => tail ~> Divide
        case POW => tail ~> Pow
        case SIN => tail ~> Sin
        case COS => tail ~> Cos
        case TAN => tail ~> Tan
        case EXP => tail ~> Exp
        case LN => tail ~> Ln
      }
    }

    implicit class ParseHelper(tokens: List[String]) {
      def ~>(ctor: (Expr, Expr) => Expr) =
        ExprParser(tokens) map2 { (left, right, parser) => (ctor(left, right), parser) }

      def ~>(ctor: Expr => Expr) =
        ExprParser(tokens) map { (expr, parser) => (ctor(expr), parser) }
    }
  }

  case class ValParser(tokens: List[String]) extends Parser(tokens) {
    override def eval() = tokens match {
      case head :: tail => (
        if (hasAlpha(head)) Var(head) else Const(head),
        ExprParser(tail)
      )
    }
  }

  def diff(expr: String): String = {
    val (newExpr: Expr, _) = parse(expr)
    val sExpr: Expr = Simplify(newExpr)
    val dExpr: Expr = Derive(sExpr)

    dExpr.toString
  }

  def parse(expr: String): (Expr, Parser) = ExprParser(tokenize(expr)).eval()

  //todo: improve this so that tokenizer and parser run together and
  //      not tokenizer first and parser second like now
  private[this] def tokenize(input: String): List[String] = input
    .split(SPACE)
    .toList
    .flatMap(part => {
      if (isBegin(part))
        List(BEGIN, strip(part))
      else if (isEnd(part))
        strip(part) :: part
          .toIndexedSeq
          .flatMap(x => if (x == ')') List(END) else List())
          .toList
      else
        List(part)
    })

  //Simplify expressions:

  case object Simplify {
    def apply(expr: Expr): Expr = expr match {
      //*****[BASE CASES]*****

      //identities (takes commutativity of sum and product into account)
      case c: Const => c
      case x: Var => x
      case Sum(Pow(_, _), _) | Sum(_, Pow(_, _)) => expr
      case Sum(Var(_), Const(_)) | Sum(Const(_), Var(_)) => expr
      case Subtract(left, Const(0)) => left //neutral element
      case Subtract(Var(_), Const(_)) => expr
      case Product(_, Const(0)) | Product(Const(0), _) => Const(0)
      case Product(left, Const(1)) => left //neutral element
      case Product(Const(1), right) => right //neutral element
      case Product(Const(_), Var(_)) | Product(Var(_), Const(_)) => expr
      case Product(_, Sin(_)) | Product(_, Cos(_)) | Product(_, Exp(_)) | Product(_, Pow(_, _)) => expr
      case Divide(Var(_), Const(_)) | Divide(Const(_), Var(_))  => expr
      case Pow(_, _) => expr
      case Ln(_) => expr

      //no further simplification needed
      case Sum(Const(c0), Const(c1)) => Const(c0 + c1)
      case Sum(x0: Var, x1: Var) if x0 == x1 => Product(Const(2), x0)
      case Subtract(x0: Var, x1: Var) if x0 == x1 => Const(0)
      case Subtract(Const(c0), Const(c1)) => Const(c0 - c1)
      case Product(x0: Var, x1: Var) if x0 == x1 => Pow(x0, Const(2))

      //complex/nested cases
      case Sum(Product(x0: Var, Const(c)), x1: Var) if x0 == x1 => Product(x0, Const(c + 1))
      case Sum(x0: Var, Product(Const(c), x1: Var)) if x0 == x1 => Product(x0, Const(c + 1))
      case Sum(Sum(left, right: Const), x: Var) => Sum(Simplify(Sum(x, left)), right) //associativity & commutativity
      case Sum(x: Var, Sum(left, right: Const)) => Sum(Simplify(Sum(x, left)), right) //associativity & commutativity
      case Subtract(Product(Const(c), x0: Var), x1: Var) if x0 == x1 => Product(x0, Const(c - 1))
      case Subtract(Sum(x0: Var, c: Const), x1: Var) if x0 == x1 => c
      case Product(Sum(x: Var, Const(c0)), Const(c1)) => Sum(Product(x, Const(c1)), Const(c0 * c1))
      case Product(Const(c1), Sum(x: Var, Const(c0))) => Sum(Product(x, Const(c1)), Const(c0 * c1))

      //*****[RECURSIVE CASES]*****

      //simplify, rebuild and simplify again
      //todo remove unnecessary simplifications
      case Sum(left, right) =>  Simplify { simplify(left, right) ~> Sum }
      case Subtract(left, right) => Simplify { simplify(left, right) ~> Subtract }
      case Product(left, right) =>  simplify(left, right) ~> Product
      case Divide(left, right) =>  simplify(left, right) ~> Divide

      case Sin(subExpr) => Simplify(subExpr) ~> Sin
      case Cos(subExpr) => Simplify(subExpr) ~> Cos
      case Tan(subExpr) => Simplify(subExpr) ~> Tan
      case Exp(subExpr) => Simplify(subExpr) ~> Exp
      case Ln(subExpr) => Simplify(subExpr) ~> Ln
    }

    case class SimplifyHelper(left: Expr, right: Expr) {
      def ~>(f: (Expr, Expr) => Expr) = f(left, right)
    }

    implicit class SimplifyHelper2(expr: Expr) {
      def ~>(f: Expr => Expr) = f(expr)
    }

    def simplify(leftRight: (Expr, Expr)): SimplifyHelper = leftRight match {
      case (left, right) => SimplifyHelper(Simplify(left), Simplify(right))
    }
  }

  case object Derive {
    def apply(expr: Expr): Expr = expr match {
      case Const(_) => Const(0)
      case Var(_) | Sum(Var(_), Const(_)) | Sum(Const(_), Var(_)) => Const(1)
      case Sum(left, Const(_)) => Derive(left)
      case Sum(Const(_), right) => Derive(right)
      case Product(Var(_), c: Const) => c
      case Product(c: Const, Var(_)) => c
      case Divide(_, Const(c)) => Const(1.0 / c)
      case Divide(g, h) => Simplify(Divide(
        Subtract(Product(Derive(g), h), Product(g, Derive(h))),
        Pow(h, Const(2))
      ))
      case Pow(base, Const(c)) => {
        val tailExpr = if (c > 2) Pow(base, Const(c-1)) else base

        Simplify { Product(Derive(base), Product(Const(c), tailExpr)) }
      }
      case Sin(subExpr) => Simplify { Product(Derive(subExpr), Cos(subExpr)) }
      case Cos(subExpr) => Simplify { Product(Derive(subExpr), Product(Const(-1), Sin(subExpr))) }
      case Tan(subExpr) => Simplify { Product(Derive(subExpr), Sum(Const(1), Pow(Tan(subExpr), Const(2)))) }
      case Exp(subExpr) => Simplify { Product(Derive(subExpr), Exp(subExpr)) }
      case Ln(subExpr) => Divide(Const(1), subExpr)

      //Recursive cases:
      case Product(c: Const, right) => Product(c, Derive(right))
    }
  }

  //Helpers:
  private[this] def isBegin(x: String) = x.startsWith(BEGIN)
  private[this] def isEnd(x: String) = x.endsWith(END)
  private[this] def strip(x: String) = x.replace(BEGIN, "").replace(END, "")

  private[this] def hasAlpha(x: String) = x.filter(Character.isAlphabetic(_)).nonEmpty
  private[this] def isAlphaNumeric(xs: String) = xs
    .filter(x => Character.isAlphabetic(x) || Character.isDigit(x))
    .nonEmpty
}
