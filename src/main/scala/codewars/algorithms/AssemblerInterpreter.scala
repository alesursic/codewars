package codewars.algorithms

import scala.annotation.tailrec

//complexity = 2kyu
//NOTE: Object oriented approach is probably better than functional-programming
object AssemblerInterpreter {
  val CMD_DELIMITER = " "
  val ARGS_DELIMITER = ", "
  val COMMENTS_DELIMITER = ";"

  def interpret(input: String): Option[String] = {
    //Parse program text into commands and labels
    val program = input
      .split("\n")
      .filter(!_.startsWith(COMMENTS_DELIMITER)) //skip lines with comments only
      .filter(_ != "") //skips empty lines

    val commands = CmdParser.parse(program)

    val labels: Map[String, Int] = commands
      .zipWithIndex
      .filter({ case (cmd :: _, _) => cmd.endsWith(":") })
      .map({ case (cmd :: _, idx) => cmd.dropRight(1) -> idx })
      .toMap

    //Build lazy interpreter
    val interpreter = Interpreter(commands, labels)

    //Interpret the program
    runWhile(
      s => interpreter.stateAction()(s).build(), //single state transition
      _ < program.size //condition
    )(
      State(0, Regs(Map.empty), List.empty, List.empty, None, false) //initial state
    ) map {
      xs => xs.foldRight("")((h, t) => h ++ t) //fold multiple messages into a single resulting message
    }
  }

  //-------------------------------------------------------------------------------------------------------------

  type Reg = String
  type Cmd = List[String]

  object CmdParser {
    def parse(program: Array[String]): Array[Cmd] =
      program.map(cmdStr => {
        val parts = trimComments(cmdStr)
          .split(CMD_DELIMITER, 2) //split command into two parts (cmd, varargs)
          .filter(_ != "") //skips empty characters (sometimes more than 1 space is used as a delimiter)
          .map(_.trim) //when splitting a string into two some trailing or leading whitespaces may have appeared

        parts(0) match {
          //varargs instructions:
          case cmd@"msg" => List(cmd, parts(1))
          //double argument instructions:
          case cmd@("mov" | "add" | "sub" | "div" | "mul" | "cmp") => cmd :: parts(1).split(ARGS_DELIMITER).toList
          //single argument instructions:
          case cmd@("inc" | "dec" | "call" | "jne" | "jl" | "jle" | "jmp" | "jg" | "je" | "jge") => List(cmd, parts(1))
          //no argument instructions:
          case cmd => List(cmd)
        }
    })
  }

  case class State(pc: Int, regs: Regs, stack: List[Int], msgs: List[String], xyCmp: Option[(Int, Int)], done: Boolean)

  case class Interpreter(program: Array[Cmd], labels: Map[String, Int]) {
    def stateAction(): State => StateBuilder = state => state match {
      //NOTE: stateBuilder.build() always increments PC by 1
      case State(pc, regs, stack, _, xyCmp, _) => {
        //take next command to execute
        program(pc) match {
          case "msg" :: varargs :: Nil => state.addMsg(tokenize(varargs).map(_.fold(identity, regs.read(_).toString)).reduce(_ + _))
          case "mov" :: x :: y :: Nil => state.setRegs(regs.move(x, ConstOrReg(y)))
          case "add" :: x :: y :: Nil => state.setRegs(regs.add(x, ConstOrReg(y)))
          case "sub" :: x :: y :: Nil => state.setRegs(regs.sub(x, ConstOrReg(y)))
          case "div" :: x :: y :: Nil => state.setRegs(regs.div(x, ConstOrReg(y)))
          case "mul" :: x :: y :: Nil => state.setRegs(regs.mul(x, ConstOrReg(y)))
          case "cmp" :: x :: y :: Nil => state.setXyCmp((ConstOrReg(x).constOrRead(regs), ConstOrReg(y).constOrRead(regs)))
          case "inc" :: x :: Nil => state.setRegs(regs.add(x, 1))
          case "dec" :: x :: Nil => state.setRegs(regs.sub(x, 1))
          case "call" :: lbl :: Nil => state.pushToStack(pc).setPc(labels(lbl))
          case "jne" :: lbl :: Nil => if (xyCmp.exists({ case (x, y) => x != y })) state.setPc(labels(lbl)) else state
          case "jl" :: lbl :: Nil => if (xyCmp.exists({ case (x, y) => x < y })) state.setPc(labels(lbl)) else state
          case "jle" :: lbl :: Nil => if (xyCmp.exists({ case (x, y) => x <= y })) state.setPc(labels(lbl)) else state
          case "jg" :: lbl :: Nil => if (xyCmp.exists({ case (x, y) => x > y })) state.setPc(labels(lbl)) else state
          case "je" :: lbl :: Nil => if (xyCmp.exists({ case (x, y) => x == y })) state.setPc(labels(lbl)) else state
          case "jge" :: lbl :: Nil => if (xyCmp.exists({ case (x, y) => x >= y })) state.setPc(labels(lbl)) else state
          case "jmp" :: lbl :: Nil => state.setPc(labels(lbl))
          case "ret" :: Nil => state.setPc(stack.head).popFromStack()
          case "end" :: Nil => state.setDone(true)
          case _ => state //NOOP (label:)
        }
      }
    }
  }

  //Helpers:

  @tailrec
  def runWhile(sa: State => State, p: Int => Boolean)(state: State): Option[List[String]] =
    state match {
      //if taking condition isn't meet, return the same state (operation is then identity or NOOP)
      case State(pc, _, _, msgs, _, done) => {
        if (done) Some(msgs)
        else if (p(pc)) runWhile(sa, p)(sa(state))
        else None
      }
    }

  /*
   * My own fucking tokenizer..
   * Input example: "'Term ', a, ' of Fibonacci series is: ', b "
   */
  def tokenize(cmd: String): List[Either[String, Reg]] = {
    var acc: List[Either[String, Reg]] = List()

    var betweenQuotes = false
    var fmt = new StringBuilder("")
    var reg = new StringBuilder("")

    for (char <- cmd) {
      if (char == '\'') {
        //formats
        if (betweenQuotes) //found an ending quote
          acc = Left(fmt.toString()) :: acc
        betweenQuotes = !betweenQuotes
        fmt = new StringBuilder("")
      } else if (!betweenQuotes && char == ',') { //delimiter
        //registers
        if (reg.size > 0)
          acc = Right(reg.toString()) :: acc
        reg = new StringBuilder("")
      } else if (betweenQuotes) {
        fmt.addOne(char)
      } else if (char != ' ') {
        reg.addOne(char)
      }
    }

    //loop finished before writing the last word into the list
    if (reg.size > 0)
      acc = Right(reg.toString()) :: acc

    acc.reverse
  }
  def trimComments(str: String): String = str.split(COMMENTS_DELIMITER)(0).trim

  implicit class StateBuilder(s: State) {
    var pc: Int = s.pc
    var regs: Regs = s.regs
    var stack: List[Int] = s.stack
    var msgs: List[String] = s.msgs
    var xyCmp: Option[(Int, Int)] = s.xyCmp
    var done: Boolean = s.done

    def setPc(pc: Int) = { this.pc = pc; this }
    def setRegs(regs: Regs) = { this.regs = regs; this }
    def pushToStack(x: Int) = { stack = x :: stack; this }
    def popFromStack() = { stack = stack.tail; this }
    def addMsg(msg: String) = { msgs = msg::msgs; this }
    def setXyCmp(xyCmp: (Int, Int)) = { this.xyCmp = Some(xyCmp); this }
    def setDone(done: Boolean) = { this.done = done; this}

    def build(): State = build(_ + 1)
    def build(incPc: Int => Int): State = State(incPc(pc), regs, stack, msgs, xyCmp, done)
  }

  case class Regs(values: Map[Reg, Int]) {
    def move(x: Reg, y: ConstOrReg): Regs = Regs(
      values + (x -> y.constOrRead(this))
    )

    def arithm(x: Reg, y: ConstOrReg, op: (Int, Int) => Int): Regs = Regs(
      values + (x -> op(values(x), y.constOrRead(this)))
    )
    def add(x: Reg, y: Int): Regs = add(x, const(y))
    def add(x: Reg, y: ConstOrReg): Regs = arithm(x, y, _ + _)
    def sub(x: Reg, y: Int): Regs = sub(x, const(y))
    def sub(x: Reg, y: ConstOrReg): Regs = arithm(x, y, _ - _)
    def div(x: Reg, y: ConstOrReg): Regs = arithm(x, y, _ / _)
    def mul(x: Reg, y: ConstOrReg): Regs = arithm(x, y, _ * _)

    def read(x: Reg): Int = values(x)

    private[this] def const(x: Int) = ConstOrReg(Left(x))
  }

  case object ConstOrReg {
    def apply(x: String): ConstOrReg = ConstOrReg(parse(x))

    private[this] def isAlpha(x: String) = x.filter(Character.isAlphabetic(_)).size == x.size
    private[this] def parse(x: String) = if (isAlpha(x)) Right(x) else Left(Integer.parseInt(x))
  }
  case class ConstOrReg(x: Either[Int, Reg]) {
    def constOrRead(regs: Regs): Int = x.fold(identity, regs.read(_))
    def test(regs: Regs, p: Int => Boolean): Boolean = p(constOrRead(regs))
  }
}
