package com.tandcode.adventofcode.y2023.d10

import com.tandcode.adventofcode.y2023.d10.PipeMaze.Pos
import com.tandcode.adventofcode.io.Util.strToLines

import scala.annotation.tailrec

object PipeMaze {
  val startChar = 'S'

  // position from top left corner
  case class Pos(y: Int, x: Int) {

    lazy val left: Pos = Pos(y, x - 1)
    lazy val right: Pos = Pos(y, x + 1)
    lazy val up: Pos = Pos(y - 1, x)
    lazy val down: Pos = Pos(y + 1, x)

    def steps: Seq[Pos] = Seq(left, right, up, down)

    def nextStep(grid: Array[String], prev: Pos, reversed: Boolean = false): Option[Pos] = {
      grid(y)(x) match
        case '|' => Some(if reversed ^ prev.y < y then down else up)
        case '-' => Some(if reversed ^ prev.x < x then right else left)
        case 'L' => Some(if reversed ^ prev.y < y || prev.x < x then right else up)
        case 'J' => Some(if reversed ^ prev.y < y || prev.x > x then left else up)
        case '7' => Some(if reversed ^ prev.y < y || prev.x < x then down else left)
        case 'F' => Some(if reversed ^ prev.y < y || prev.x > x then down else right)
        case _ => None
    }.filter { _.validForGrid(grid) }

    def validForGrid(grid: Array[String]): Boolean = y >= 0 && y < grid.length && x >= 0 && y < grid(0).length
  }

  def part1(input: String): Int = {
      val grid = strToLines(input).toArray
      gridToPath(grid).size / 2
  }

  private def gridToPath(grid: Array[String]) = {
    val start = startPos(grid)

    start.steps
      .filter(_.validForGrid(grid)).find(p => p.nextStep(grid, start, true).contains(start)).toList
      .flatMap(pos => toPath(grid, pos, start, List(pos)))
  }

  @tailrec
  def toPath(grid: Array[String], pos: Pos, prev: Pos, path: List[Pos]): List[Pos] = {
    if grid(pos) == startChar then path else {
      pos.nextStep(grid, prev) match
        case Some(nextPos) => toPath(grid, nextPos, pos, nextPos :: path)
        case _ => Nil
    }
  }

  def startPos(grid: Array[String]): Pos = {
    val startY = grid.indexWhere(_.indexOf(startChar) >= 0)
    Pos(startY, grid(startY).indexOf(startChar))
  }

  def startSym(grid: Array[String], p: Pos): Char = {
    val vertUp = Set('|', 'F', '7')(grid(p.up))
    val vertDown = Set('|', 'J', 'L')(grid(p.down))
    val horLeft = Set('-', 'F', 'L')(grid(p.left))
    val horRight = Set('-', '7', 'J')(grid(p.right))

    if vertUp && vertDown then '|'
    else if horLeft && horRight then '-'
    else if horLeft && vertDown then '7'
    else if horLeft && vertUp then 'J'
    else if horRight && vertDown then 'F'
    else if horRight && vertUp then 'L'
    else 'S'
  }

  def part2(input: String): Int = {
    val grid = strToLines(input).toArray
    val path = gridToPath(grid).toSet
    val startPosition = startPos(grid)
    val startSymbol = startSym(grid, startPosition)

    val borderRex = Seq("\\|", "F-*J", "L-*7").map(_.r)
    val gridClean = grid.map(_.replace('S', startSymbol))

    val insideTiles = for {
      y <- gridClean.indices
      x <- gridClean(y).indices
      p = Pos(y, x)
      if !path(p)
      left = gridClean(y).substring(x)
      matches = borderRex.flatMap(_.findAllMatchIn(left)).filter(xx => path(Pos(y, xx.start + x)))
      totalBorderIntersections = matches.length
      insideCircle = totalBorderIntersections % 2 == 1
      if insideCircle
    } yield p

    insideTiles.size
  }

  implicit class ReachGrid(grid: Array[String]) {
    def apply(pos: Pos): Char = grid(pos.y)(pos.x)
  }
}
