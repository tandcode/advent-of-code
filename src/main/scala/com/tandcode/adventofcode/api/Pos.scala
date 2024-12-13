package com.tandcode.adventofcode.api

import com.tandcode.adventofcode.api
import com.tandcode.adventofcode.api.Direction._
import com.tandcode.adventofcode.y2023.d10.PipeMaze.Pos

case class Pos(y: Int, x: Int) {

  lazy val left: Pos = Pos(y, x - 1)
  lazy val right: Pos = Pos(y, x + 1)
  lazy val up: Pos = Pos(y - 1, x)
  lazy val down: Pos = Pos(y + 1, x)
  
  def apply(dir: Direction): Pos = dir match
    case Up => up
    case Left => left
    case Down => down
    case Right => right

  def steps: Seq[Pos] = Seq(left, right, up, down)
  def validSteps(grid: Array[Array[Int]]): Seq[Pos] = steps.filter(_.validForGrid(grid))
  def validSteps(grid: Array[String]): Seq[Pos] = steps.filter(_.validForGrid(grid))

  def validForGrid(grid: Array[String]): Boolean = y >= 0 && y < grid.length && x >= 0 && x < grid(0).length
  def validForGrid(grid: Array[Array[Int]]): Boolean = y >= 0 && y < grid.length && x >= 0 && x < grid(0).length
}
