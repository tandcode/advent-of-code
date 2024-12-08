package com.tandcode.adventofcode.api

case class HeadingPos(pos: Pos, dir: Direction) {
  def apply(newDir: Direction): HeadingPos = HeadingPos(pos(newDir), newDir)

  def validForGrid(grid: Array[String]): Boolean = pos.validForGrid(grid)
  def validForGrid(grid: Array[Array[Int]]): Boolean = pos.validForGrid(grid)
}
