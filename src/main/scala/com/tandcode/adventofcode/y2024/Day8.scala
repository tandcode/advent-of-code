package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.{Pos, ReachGridStr, ReachString}

import scala.annotation.tailrec

object Day8 {

  def part1(input: String): Long = antiNodes(input, maxAntiNodes = 1, withAntennas = false)

  def part2(input: String): Long = antiNodes(input, maxAntiNodes = Int.MaxValue, withAntennas = true)

  private def antiNodes(input: String, maxAntiNodes: Int, withAntennas: Boolean): Long = {
    val grid = input.slines
    val antennaToPositions = grid.uniquePositions('.')
    val totalAntiNodes = antennaToPositions.flatMap(ans => {
      ans._2.combinations(2).flatMap(combs => {
        val Seq(p1, p2) = combs.toSeq
        allAntennasFor(p1, p2, maxAntiNodes)(grid)
      })
    }).toSet
    if withAntennas then (totalAntiNodes ++ antennaToPositions.flatMap(_._2)).size else totalAntiNodes.size
  }

  private def allAntennasFor(p1: Pos, p2: Pos, upTo: Int)(grid: Array[String]): Seq[Pos] = {
    val dx = p2.x - p1.x
    val dy = p2.y - p1.y
    
    @tailrec
    def inner(prev: Pos, dy: Int, dx: Int, i: Int = 0, antiNodes: Seq[Pos] = Nil): Seq[Pos] = {
      val next = Pos(prev.y + dy, prev.x + dx)
      if i < upTo && next.validForGrid(grid) then inner(next, dy, dx, i + 1, next +: antiNodes) else antiNodes 
    }
    inner(p1, -dy, -dx) ++ inner(p2, dy, dx)
  }
}
