package com.tandcode.adventofcode.y2023.d09

import com.tandcode.adventofcode.io.Util.strToLines

import scala.annotation.tailrec

object MirageMaintenance {

  private def processValuesWithTransform(input: String, transform: List[Long] => List[Long]) = {
    val lines = strToLines(input)
      .map(line => transform(line.split("\\s+").map(_.toLong).toList))

    @tailrec
    def loop(difs: List[Seq[Long]]): Long =
      if difs.head.forall(_ == 0)
      then difs.tail.foldLeft(0L) { case (res, next) => next.head + res }
      else loop(difs.head.sliding(2).map { case Seq(a, b) => a - b }.toSeq :: difs)

    lines
      .map(nums => loop(List(nums)))
      .sum
  }

  def part1(input: String): Long = processValuesWithTransform(input, _.reverse)

  def part2(input: String): Long = processValuesWithTransform(input, identity)
}
