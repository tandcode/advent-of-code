package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.Direction.*
import com.tandcode.adventofcode.api.{Direction, ReachString}

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.util.matching.Regex

object Day17 {

  def part1(input: String): String = {
    val numPattern: Regex = "(\\d+)".r
    val Array(registers, programs) = input.parts()
    val Array(a, b, c) = registers.slines.map { reg => numPattern.findFirstIn(reg).get.toInt }
    val program: Array[Int] = numPattern.findAllIn(programs).toArray.map(_.toInt)
    @tailrec
    def inner(state: State): State = {
      if state.p >= program.length then state else inner(Opcode(program(state.p))(program(state.p + 1), state))
    }

    val state = inner(State(a, b, c, 0, Nil))
    state.res.mkString(",")
  }

  def part2(input: String): String = {
    val numPattern: Regex = "(\\d+)".r
    val Array(registers, programs) = input.parts()
    val Array(a, b, c) = registers.slines.map { reg => numPattern.findFirstIn(reg).get.toInt }
    val program: Array[Int] = numPattern.findAllIn(programs).toArray.map(_.toInt)

    @tailrec
    def inner(state: State): Option[State] = {
      val sameLastElem = state.res.isEmpty || state.res.last == program(state.res.size - 1)
      val endOfProgram = state.p >= program.length
      val outSameAsProgram = state.res.size == program.length

      if !sameLastElem || (endOfProgram && !outSameAsProgram)
      then {
        val matching = state.res.size - 7
        if (matching > 0 ) {
          println(s"matching $matching")
        }
        return None
      }
      if endOfProgram
      then Some(state)
      else inner(Opcode(program(state.p))(program(state.p + 1), state))
    }

    def inner2(i: Long): Long = {
      val maybeState = inner(State(i, b, c, 0, Nil))
      maybeState match
        case Some(state) => i
        case None => {
          if (i != 0 && i % 10_000_000 == 0) {
            println(s"i = $i")
          }
          inner2(i + 1)
        }
    }
    val aa = inner2(0)
    aa.toString
  }

  case class State(a: Long, b: Long, c: Long, p: Int, res: Seq[Long]) {
    def combo(operand: Int): Long = operand match
      case 4 => a
      case 5 => b
      case 6 => c
      case 7 => throw new IllegalArgumentException("Invalid operand")
      case o => o
  }

  case class RevState(a: Int, b: Int, c: Int, p: Int, steps: LazyList[Int]) {
    def uncombo(combo: Int): List[Int] = {
      val res = new mutable.ArrayBuffer[Int]
      res ++= (if combo < 4 then List(0, 1, 2, 3) else Nil)
      if combo == a then res += 4
      if combo == b then res += 5
      if combo == c then res += 6
      res.toList
    }
  }

  enum Opcode(val code: Int, app: (Int, State) => State):

    def apply(operand: Int, state: State): State = app(operand, state)
    case Adv extends Opcode(0, (operand, state) => {
      val newA = state.a / (1L << state.combo(operand))
      state.copy(a = newA, p = state.p + 2)
    })
    case Bxl extends Opcode(1, (operand, state) => {
      val newB = state.b ^ operand
      state.copy(b = newB, p = state.p + 2)
    })
    case Bst extends Opcode(2, (operand, state) => {
      val newB = (state.combo(operand) % 8)
      state.copy(b = newB, p = state.p + 2)
    })
    case Jnz extends Opcode(3, (operand, state) => {
      if state.a == 0 then state.copy(p = state.p + 2) else state.copy(p = operand)
    })
    case Bxc extends Opcode(4, (operand, state) => {
      val newB = state.b ^ state.c
      state.copy(b = newB, p = state.p + 2)
    })
    case Out extends Opcode(5, (operand, state) => {
      val out = state.combo(operand) % 8
      state.copy(p = state.p + 2, res = state.res :+ out)
    }
    )
    case Bdv extends Opcode(6, (operand, state) => {
      val newB = state.a / (1 << state.combo(operand))
      state.copy(b = newB, p = state.p + 2)
    })
    case Cdv extends Opcode(7, (operand, state) => {
      val newC = state.a / (1 << state.combo(operand))
      state.copy(c = newC, p = state.p + 2)
    })
    
  object Opcode {
    def apply(code: Int): Opcode = values.find(_.code == code).get
  }

}