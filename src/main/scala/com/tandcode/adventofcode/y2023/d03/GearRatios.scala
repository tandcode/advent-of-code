package com.tandcode.adventofcode.y2023.d03

import scala.io.Source
import scala.util.Using

object GearRatios extends App {

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

  val testValues: Seq[String] = Seq(
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  )
  println(s"Test ratios: ${
    sumUpPartNumbers(testValues)
  }")

  val inputValues: Seq[String] = Using(Source.fromResource("y2023/d03/input.txt"))(_.getLines().toSeq).getOrElse(Nil)

  println(s"Actual gear ratios: ${sumUpPartNumbers(inputValues)}")

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

  println(s"Test product gear ratios: ${gearRatios(testValues)}")
  println(s"Actual product gear ratios: ${gearRatios(inputValues)}")

}
