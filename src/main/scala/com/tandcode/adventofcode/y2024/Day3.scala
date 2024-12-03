package com.tandcode.adventofcode.y2024

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object Day3 {

  val mulRex = "mul\\((\\d+),(\\d+)\\)".r

  def part1(input: String): Long = calculate(Seq(input))

  def part2(input: String): Long = calculate(onlyDo(input))

  def calculate(line: Seq[String]): Long = line.flatMap(l => mulRex.findAllMatchIn(l))
    .map(m => m.subgroups match {
      case num1 :: num2 :: Nil => num1.toLong * num2.toLong
    })
    .sum

  def onlyDo(line: String, i: Int = 0, isDo: Boolean = true, ranges: List[String] = Nil): Seq[String] =
    if (i >= line.length) return ranges
    val nextIFn = (op: String) => Option(line.indexOf(op, i)).filterNot(_ == -1).getOrElse(line.length)
    val i2 = nextIFn(if isDo then "don't()" else "do()")
    onlyDo(line, i2, !isDo, if isDo then line.substring(i, i2) :: ranges else ranges)

  // keep it for history :)
  def onlyDoINCORRECT(line: String): Seq[String] = {
    var lastDoI = 0
    var lastDontI = 0
    var i = 0
    val ranges = new mutable.ListBuffer[String]
    while (i < line.length) {
      val isDont = line.startsWith("don't()", i)
      val isDo = line.startsWith("do()", i)
      if (isDont) {
        lastDontI = i
      } else if (isDo) {
        if (lastDontI > lastDoI) {
          ranges += line.substring(lastDoI, lastDontI)
        }
        if (lastDontI != 0) {
          lastDoI = i
        }
      }
      i += 1
    }
    if (lastDontI > lastDoI) {
      ranges += line.substring(lastDoI, lastDontI)
    } else {
      ranges += line.substring(lastDoI, line.length)
    }
    ranges.toSeq
  }
}
