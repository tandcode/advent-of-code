package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.io.ReachString
import com.tandcode.adventofcode.io.ReachStrings

object Day5 {

  def part1(input: String): Long =
    val (pages, ord) = parseInput(input)
    implicit val impOrd: Ordering[Int] = ord
    calculateInstructionSum(pages, _.filter(isSorted))

  def part2(input: String): Long =
    val (pages, ord) = parseInput(input)
    implicit val impOrd: Ordering[Int] = ord
    calculateInstructionSum(pages, _.filterNot(isSorted).map(_.sorted))

  private def parseInput(input: String): (Array[Array[Int]], Ordering[Int]) = {
    val Array(orderingRules, updates) = input.parts()
    val orderingMap = orderingRules.slines
      .map(or => {
        val Array(smaller, bigger) = or.split("\\|").numbers
        (smaller, bigger)
      })
      .groupMap(_._1)(_._2)
      .map((k, v) => k -> v.toSet)

    val anOrdering: Ordering[Int] = (p1, p2) =>
      if orderingMap.get(p1).exists(_(p2))
      then -1
      else if orderingMap.get(p2).exists(_(p1))
      then 1
      else 0

    (updates.slines.map(_.split(",").numbers), anOrdering)
  }

  def calculateInstructionSum(pages: Array[Array[Int]],
                              allPagesMapping: Array[Array[Int]] => Array[Array[Int]]): Int = {
    allPagesMapping(pages)
      .map(pages => pages(pages.length / 2))
      .sum
  }

  private def isSorted[T](s: Seq[T])(implicit ord: Ordering[T]): Boolean = s match {
    case Seq() => true
    case Seq(_) => true
    case _ => s.sliding(2).forall { case Seq(x, y) => ord.lteq(x, y) }
  }

}
