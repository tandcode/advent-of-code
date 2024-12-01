package com.tandcode.adventofcode.y2023.d01

import com.tandcode.adventofcode.io.Util.strToLines

object Trebuchet {

  def part1(input: String): Int = {
    calibrate(strToLines(input))
  }

  def part2(input: String): Int = {
    calibrateImproved(strToLines(input))
  }
  
  def calibrate(codes: Seq[String]): Int  = {
    val calibrationValues = codes.flatMap(line =>
      for {
        first <- line.find(_.isDigit)
        last <- line.findLast(_.isDigit)
      } yield s"$first$last".toInt
    )

    calibrationValues.sum
  }

  def calibrateImproved(codes: Seq[String]): Int = {
    val numbers = Map(
      "1" -> 1, "one" -> 1,
      "2" -> 2, "two" -> 2,
      "3" -> 3, "three" -> 3,
      "4" -> 4, "four" -> 4,
      "5" -> 5, "five" -> 5,
      "6" -> 6, "six" -> 6,
      "7" -> 7, "seven" -> 7,
      "8" -> 8, "eight" -> 8,
      "9" -> 9, "nine" -> 9,
      "0" -> 0, "zero" -> 0,
    )
    val calibrationValues = codes.map(line =>
      val skipNegative: Int => Int = i => if i < 0 then Int.MaxValue else i
      val first = numbers.minBy { case (num, _) => skipNegative(line.indexOf(num)) }._2
      val last = numbers.maxBy { case (num, _) => line.lastIndexOf(num) }._2
      first * 10 + last
    )

    calibrationValues.sum
  }

}
