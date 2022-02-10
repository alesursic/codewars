package codewars.algorithms

import scala.annotation.tailrec

//complexity = 5 kyu
/**
 * SUPPORTED COMMANDS
 *
 * mov x y - copies y (either a constant value or the content of a register) into register x
 * inc x - increases the content of the register x by one
 * dec x - decreases the content of the register x by one
 * jnz x y - jumps to an instruction y steps away
 *  (positive means forward, negative means backward, y can be a register or a constant),
 *  but only if x (a constant or a register) is not zero
 *
 */
//  case class Interpreter(program: Array[Cmd], pc: Int, registers: Map[String, Int])
object SimpleAssembler {
  def interpret(program: List[String]): Map[String, Int] = {
    val interpreter = Interpreter(
      program.map(cmdStr => {
        val parts = cmdStr.split(" ")
        val x = parts(1)

        parts(0) match {
          case "mov" => Mov(x, parse(parts(2)))
          case "inc" => Inc(x)
          case "dec" => Dec(x)
          case "jnz" => Jnz(parse(x), parse(parts(2)))
        }
      }).toArray
    )

    val result = takeWhileTailRec(interpreter.stateAction(), _ < program.size)(InterpreterState(0, Map.empty))

    result.registers
  }

  sealed trait Cmd
  case class Mov(x: RegAddr, y: Either[Const, RegAddr]) extends Cmd
  case class Inc(x: RegAddr) extends Cmd
  case class Dec(x: RegAddr) extends Cmd
  case class Jnz(x: Either[Const, RegAddr], y: Either[Const, RegAddr]) extends Cmd

  type PC = Int
  type Const = Int
  type RegAddr = String
  type Registers = Map[RegAddr, Int]
  type Program = Array[Cmd]

  case class InterpreterState(pc: PC, registers: Registers)

  case class Interpreter(program: Program) {
    def stateAction(): InterpreterState => InterpreterState = state => state match {
      case InterpreterState(pc, registers) => {
        //take next command to execute
        program(pc) match {
          case Mov(x, y) => {
            val newRegisters = registers + (x -> y.fold(identity, registers(_)))
            InterpreterState(pc + 1, newRegisters)
          }
          case Inc(x) => {
            val newRegisters = registers + (x -> (registers(x) + 1))
            InterpreterState(pc + 1, newRegisters)
          }
          case Dec(x) => {
            val newRegisters = registers + (x -> (registers(x) - 1))
            InterpreterState(pc + 1, newRegisters)
          }
          case Jnz(x, y) => {
            val jmpVal = y.fold(identity, registers(_))
            InterpreterState(if (x.fold(identity, registers(_)) == 0) pc + 1 else pc + jmpVal, registers)
          }
        }
      }
    }
  }

  //Helpers:

  def takeWhile(sa: InterpreterState => InterpreterState, p: Int => Boolean): InterpreterState => InterpreterState =
    state => state match {
      //if taking condition isn't meet, return the same state (operation is then identity or NOOP)
      case InterpreterState(pc, _) => if (p(pc)) takeWhile(sa, p)(sa(state)) else state
    }

  @tailrec
  def takeWhileTailRec(sa: InterpreterState => InterpreterState, p: Int => Boolean)(state: InterpreterState): InterpreterState =
    state match {
      //if taking condition isn't meet, return the same state (operation is then identity or NOOP)
      case InterpreterState(pc, _) => if (p(pc)) takeWhileTailRec(sa, p)(sa(state)) else state
    }

  private[this] def isAlpha(x: String) = x.filter(Character.isAlphabetic(_)).size == x.size
  private[this] def parse(x: String) = if (isAlpha(x)) Right(x) else Left(Integer.parseInt(x))
}
