package com.tandcode.adventofcode.y2024
import com.tandcode.adventofcode.api.{ReachGridInt, ReachString, ReachStrings}
import com.tandcode.adventofcode.api.Pos

object Day10 {

  def part1(input: String): Long = {
    val grid = input.slines.map(_.split("").numbers)
    val startPositions = grid.findPos(0)
    startPositions.map(p => countRoutes(grid, Seq(p)).toSet.size).sum
  }

  def part2(input: String): Long = {
    val grid = input.slines.map(_.split("").numbers)
    val startPositions = grid.findPos(0)
    countRoutes(grid, startPositions).size
  }

  def countRoutes(grid: Array[Array[Int]], positions: Seq[Pos]): Seq[Pos] = {
    val ends = positions.filter(p => grid(p) == 9)
    val moreEnds = positions.flatMap(p => {
      countRoutes(grid, p.steps.filter(next => next.validForGrid(grid) && grid(p) <= 9 && grid(next) == grid(p) + 1))
    })
    ends ++ moreEnds
  }

}