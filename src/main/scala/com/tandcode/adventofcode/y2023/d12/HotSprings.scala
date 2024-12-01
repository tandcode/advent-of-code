package com.tandcode.adventofcode.y2023.d12

import com.tandcode.adventofcode.io.Util.strToLines

import scala.annotation.tailrec

object HotSprings {

  val rex = "\\?+|#+|\\.+".r

  val gapRex = "1+0+1+".r
  val rangeRex = "1+".r

  def part1(input: String): Long = countWithUnfold(input)

  def part2(input: String): Long = countWithUnfold(input, 5)

  private def countWithUnfold(input: String, unfold: Int = 1) = {
    @tailrec
    def loop(lines: Seq[String], sum: Long): Long = {
      if lines.isEmpty then sum else loop(lines.tail, sum + lineToArrangements(lines.head, unfold))
    }

    loop(strToLines(input), 0L)
  }

  private def lineToArrangements(line: String, unfold: Int): Int = {
    val Array(springs, operationalRanges) = line.split(" ")

    val springsUnf = List.fill(unfold)(springs).mkString("?")

    val operationals = operationalRanges.split(",").map(_.toInt)
    val operationalsUnf = List.fill(unfold)(operationals).flatten

    val arrangements = operationalsUnf.indices
      .map(i => {
        val before = operationalsUnf.slice(0, i)
        val after = operationalsUnf.slice(i + 1, operationalsUnf.length + 1)
        val cur = operationalsUnf(i)

        val startI = before.foldLeft((0)) { case (pos, bef) =>
          val nextPos = operationalPosition(springsUnf, pos, bef)
          nextPos
        }

        val lasti = springsUnf.length - after.lastOption.getOrElse(0)
        val reverse = after.reverse
        val endI = springsUnf.length - (reverse.foldLeft(0) { case (pos, after) =>
          val nextPos = operationalPosition(springsUnf.reverse, pos, after)
          nextPos
        })

        val possibleArrangements = startI to (endI - cur) flatMap { pos =>
          Option.when(mayOperate(springsUnf, pos, cur))(rangeToBitMask(pos, cur))
        }
        // val binaries = possibleArrangements.map(r => s"${String.format("%17s", r.toString(2))}")
        possibleArrangements

      })

    val combinations = combine(arrangements)

    val mask = toBigDecimalMask(springsUnf)

    val combinationsWithBrokenSprings = combinations
      .filter(range => (mask ^ (mask & range)) == 0)

    // println(s"arangements of ${String.format("%20s", springs)} with $unfold unfolds: $size")
    combinationsWithBrokenSprings.size
  }

  def rangeToBitMask(i: Int, len: Int): BigInt = (i until i + len).map(p => BigInt(1) << p).reduce(_ | _)

  private def operationalPosition(springsState: String, pos: Int, len: Int): Int = {
    val rangeToScan = pos to springsState.length
    val nextPosition = rangeToScan.find(pos => mayOperate(springsState, pos, len)).get
    nextPosition + len + 1
  }

  private def mayOperate(springsState: String, index: Int, len: Int) = {
    val beforeValid = index == 0 || springsState(index - 1) != '#'
    val afterValid = index + len >= springsState.length || springsState(index + len) != '#'
    val allCharsValid = (index until index + len)
      .forall(i => {
        val c = springsState(i)
        c == '#' || c == '?'
      })

    beforeValid && afterValid && allCharsValid
  }

  def combine(ranges: Seq[Seq[BigInt]]): Seq[BigInt] = {
    ranges.tail.foldLeft(ranges.head.map(r => r)) { case (res, next) =>
      res.flatMap(r => {
        next.flatMap(n => {
          Option.when(n > r && (r & n) == 0 && ((r << 1) & n) == 0)(r | n)
        })
      })
    }
  }

  def toBigDecimalMask(springsUnf: String): BigInt = {
    BigInt(springsUnf.reverse.map(c => if c == '#' then '1' else '0'), 2)
  }


}
