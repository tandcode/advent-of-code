package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.ReachString
import com.tandcode.adventofcode.y2024.Day6.Direction
import com.tandcode.adventofcode.y2024.Day6.Direction.{Down, Left, Right, Up}

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
    val next = pos.nextPos(dir)
    if !next.validForGrid(grid) then return visited + pos
    val isNextObstacle = grid(next) == '#'
    val nextDir = if isNextObstacle then turnRight(dir) else dir
    val nextPos = pos.nextPos(nextDir)
    visitedPositions(grid, nextPos, nextDir, visited + pos)
  }

  @tailrec
  def isDeadLoopWithObstacle(grid: Array[String],
                             pos: Pos,
                             dir: Direction,
                             obst: Pos,
                             visited: Set[(Pos, Direction)] = Set.empty): Boolean = {
    if visited.apply(pos -> dir) then return true
    val next = pos.nextPos(dir)
    if !next.validForGrid(grid) then return false
    val isNextObstacle = grid(next) == '#' || next == obst
    val (nextDir, nextPos) = if isNextObstacle then (dir.turnRight, pos) else (dir, pos.nextPos(dir))
    isDeadLoopWithObstacle(grid, nextPos, nextDir, obst, visited + (pos -> dir))
  }

  def turnRight(dir: Direction): Direction = dir match
    case Up => Right
    case Left => Up
    case Down => Left
    case Right => Down

  implicit class ReachGridx(grid: Array[String]) {
    def apply(pos: Pos): Char = grid(pos.y)(pos.x)
    def find(c: Char): Pos = {
      for {
        y <- grid.indices
        x <- grid(y).indices
        if grid(y)(x) == c
      } yield Pos(y, x)
    }.head

  }

  case class Pos(y: Int, x: Int) {

    lazy val left: Pos = Pos(y, x - 1)
    lazy val right: Pos = Pos(y, x + 1)
    lazy val up: Pos = Pos(y - 1, x)
    lazy val down: Pos = Pos(y + 1, x)

    def nextPos(dir: Direction): Pos = dir match
      case Up => up
      case Left => left
      case Down => down
      case Right => right

    def validForGrid(grid: Array[String]): Boolean = y >= 0 && y < grid.length && x >= 0 && x < grid(0).length
  }

  enum Direction:
    case Up, Left, Down, Right

    def turnRight: Direction = this match
      case Up => Right
      case Left => Up
      case Down => Left
      case Right => Down
}
