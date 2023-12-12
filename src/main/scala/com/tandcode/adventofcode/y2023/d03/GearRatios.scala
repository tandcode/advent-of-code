package com.tandcode.adventofcode.y2023.d03

import com.tandcode.adventofcode.y2023.io.Util.strToLines

object GearRatios {
  
  def part1(input: String): Int = sumUpPartNumbers(strToLines(input))

  def part2(input: String): Int = gearRatios(strToLines(input))

  case class NumberInfo(line: Int, num: Int, start: Int, end: Int)
  case class Gear(line: Int, start: Int)

  val numRex = "\\d+".r
  val gearRex = "\\*".r

  def sumUpPartNumbers(input: Seq[String]) = {
    input.zipWithIndex
      .flatMap { case (lineStr, lineId) => numRex.findAllMatchIn(lineStr)
        .map(m => NumberInfo(lineId, m.group(0).toInt, m.start, m.end))
      }
      .withFilter(info => {
        val indexes = info.start - 1 to info.end
        indexes.exists(i => {
          (input.lift(info.line - 1) ++ input.lift(info.line + 1) ++ input.lift(info.line))
            .exists(_.lift(i).exists(ch => !ch.isDigit && ch != '.'))
        })
      })
      .map(_.num)
      .sum
  }

  def gearRatios(input: Seq[String]) = {
    val lines = input.zipWithIndex
    val numbers = lines
      .map { case (lineStr, lineId) => numRex.findAllMatchIn(lineStr)
        .map(m => NumberInfo(lineId, m.group(0).toInt, m.start, m.end)).toSeq
      }.toArray

    val gears = lines
      .flatMap { case (lineStr, lineId) => gearRex.findAllMatchIn(lineStr).toSeq
        .map(m => Gear(lineId, m.start))
      }

    gears
      .flatMap(gear => {
        val gearRatios = (numbers.lift(gear.line - 1) ++ numbers.lift(gear.line + 1) ++ numbers.lift(gear.line))
          .flatten
          .filter(n => gear.start >= n.start - 1 && gear.start <= n.end)
          .map(_.num)
        Option.when(gearRatios.size == 2)(gearRatios.product)
      }).sum
  }

}
