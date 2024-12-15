package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.{Pos, ReachString}
import com.tandcode.adventofcode.api.ReachGridStr
import com.tandcode.adventofcode.api.Direction
import com.tandcode.adventofcode.api.Direction._

import scala.annotation.tailrec

object Day6 {

  def part1(input: String): Long = {
    val grid = input.slines
    val start = grid.find('^')

    visitedPositions(grid, start, Up).size
  }

  def part2(input: String): Long = {
    val grid: Array[String] = input.slines
    val start = grid.find('^')

    (visitedPositions(grid, start, Up) - start)
      .count(pos => isDeadLoopWithObstacle(grid, start, Up, pos))
  }

  @tailrec
  def visitedPositions(grid: Array[String], pos: Pos, dir: Direction, visited: Set[Pos] = Set.empty): Set[Pos] = {
    val next = pos(dir)
    if !next.validForGrid(grid) then return visited + pos
    val isNextObstacle = grid(next) == '#'
    val nextDir = if isNextObstacle then dir.turnRight else dir
    val nextPos = pos(nextDir)
    visitedPositions(grid, nextPos, nextDir, visited + pos)
  }

  @tailrec
  def isDeadLoopWithObstacle(grid: Array[String],
                             pos: Pos,
                             dir: Direction,
                             obst: Pos,
                             visited: Set[(Pos, Direction)] = Set.empty): Boolean = {
    if visited.apply(pos -> dir) then return true
    val next = pos(dir)
    if !next.validForGrid(grid) then return false
    val isNextObstacle = grid(next) == '#' || next == obst
    val (nextDir, nextPos) = if isNextObstacle then (dir.turnRight, pos) else (dir, pos(dir))
    isDeadLoopWithObstacle(grid, nextPos, nextDir, obst, visited + (pos -> dir))
  }

}
