package com.tandcode.adventofcode.y2023.d13

import com.tandcode.adventofcode.y2023.io.Util.strToLines

import scala.annotation.tailrec

object PointOfIncidence {

  def part1(input: String): Long = countMirrorsWithSmudges(input)

  def part2(input: String): Long = countMirrorsWithSmudges(input, 1)

  private def countMirrorsWithSmudges(input: String, smudges: Int = 0) = {

    val countYSmudges: (Array[String], Int, Int) => Int = (grid, y1, y2) =>
      grid(0).indices.count(i => grid(y1)(i) != grid(y2)(i))

    val countXSmudges: (Array[String], Int, Int) => Int = (grid, x1, x2) =>
      grid.indices.count(i => grid(i)(x1) != grid(i)(x2))

    input.split(s"(${System.lineSeparator()}){2}")
      .map(gridStr => {
        val grid = strToLines(gridStr).toArray
        checkCoordMirror(grid, 1, countYSmudges, _.yInRange, smudges)
          .map(_ * 100)
          .orElse(checkCoordMirror(grid, 1, countXSmudges, _.xInRange, smudges))
          .getOrElse(0)
      }).sum
  }

  @tailrec
  def checkCoordMirror(grid: Array[String],
                       coord: Int,
                       countSmudges: (Array[String], Int, Int) => Int,
                       coordInRange: Array[String] => Int => Boolean,
                       smudges: Int): Option[Int] = {
    @tailrec
    def loop(coord1: Int, coord2: Int, totalSmudges: Int): Boolean =
      val outOfRange = !coordInRange(grid)(coord1) || !coordInRange(grid)(coord2)
      if totalSmudges == smudges && outOfRange then true
      else if outOfRange then false
      else {
        val countedSmudges = countSmudges(grid, coord1, coord2) + totalSmudges
        if countedSmudges <= smudges then loop(coord1 - 1, coord2 + 1, countedSmudges)
        else false
      }

    if coordInRange(grid)(coord)
      then if loop(coord - 1, coord, 0) then Some(coord)
      else checkCoordMirror(grid, coord + 1, countSmudges, coordInRange, smudges)
    else None
  }

  implicit class ReachGrid(grid: Array[String]) {
    def xInRange(x: Int): Boolean = x >= 0 && x < grid(0).length
    def yInRange(y: Int): Boolean = y >= 0 && y < grid.length
  }

}
