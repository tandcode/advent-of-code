package com.tandcode.adventofcode.api

import com.tandcode.adventofcode.api
import com.tandcode.adventofcode.api.Direction.*

case class Pos(y: Int, x: Int) {

  lazy val left: Pos = apply(Left)
  lazy val right: Pos = apply(Right)
  lazy val up: Pos = apply(Up)
  lazy val down: Pos = apply(Down)
  
  def apply(dir: Direction, n: Int = 1): Pos = dir match
    case Left => Pos(y, x - n)
    case Right => Pos(y, x + n)
    case Up => Pos(y - n, x)
    case Down => Pos(y + n, x)
    

  def steps: Seq[Pos] = Seq(left, right, up, down)
  def validSteps(grid: Array[Array[Int]]): Seq[Pos] = steps.filter(_.validForGrid(grid))
  def validSteps(grid: Array[String]): Seq[Pos] = steps.filter(_.validForGrid(grid))

  def validForGrid(grid: Array[String]): Boolean = y >= 0 && y < grid.length && x >= 0 && x < grid(0).length
  def validForGrid(grid: Array[Array[Int]]): Boolean = y >= 0 && y < grid.length && x >= 0 && x < grid(0).length
}
