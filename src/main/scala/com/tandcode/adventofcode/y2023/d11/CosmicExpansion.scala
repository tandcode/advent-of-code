package com.tandcode.adventofcode.y2023.d11

import com.tandcode.adventofcode.io.Util.strToLines

object CosmicExpansion {

  // position from top left corner
  case class Pos(y: Int, x: Int):
    def lengthWithExpansion(p: Pos, expX: Seq[Int], expY: Seq[Int], times: Long): Long = {
      val xMin = x min p.x
      val xMax = x max p.x
      val yMin = y min p.y
      val yMax = y max p.y
      val expsX = expX.count(x => x > xMin && x < xMax)
      val expsY = expY.count(y => y > yMin && y < yMax)
      xMax - xMin + yMax - yMin + ((times - 1L) * (expsX + expsY))
    }

  def galaxyLengths(input: String, times: Int): Long = {
    val grid = strToLines(input).toArray
    val galaxies = for {
      y <- grid.indices
      x <- grid(y).indices
      if grid(y)(x) == '#'
    } yield Pos(y, x)

    val expandingInY = grid.zipWithIndex
      .filter(_._1.forall(_ == '.'))
      .map(_._2)
    val expandingInX = grid(0).indices
      .filter(x => grid.forall(_(x) == '.'))

    galaxies.combinations(2).toList
      .map { case Seq(g1, g2) => g1.lengthWithExpansion(g2, expandingInX, expandingInY, times) }
      .sum
  }

  def part1(input: String): Long = galaxyLengths(input, 2)

  def part2(input: String): Long = galaxyLengths(input, 1000000)

}
