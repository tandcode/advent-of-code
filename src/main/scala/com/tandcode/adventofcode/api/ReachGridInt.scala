package com.tandcode.adventofcode.api

implicit class ReachGridInt(grid: Array[Array[Int]]) {
  
  def apply(pos: Pos): Int = grid(pos.y)(pos.x)
  def apply(pos: HeadingPos): Int = apply(pos.pos)
  
}