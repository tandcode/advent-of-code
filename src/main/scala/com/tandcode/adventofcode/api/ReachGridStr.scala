package com.tandcode.adventofcode.api

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

implicit class ReachGridStr(grid: Array[String]) {
  
  def apply(pos: Pos): Char = grid(pos.y)(pos.x)

  def apply(pos: HeadingPos): Char = apply(pos.pos)
  
  def positionsOf(c: Char): Seq[Pos] = positions.filter(p => grid(p) == c)
  
  def uniquePositions(except: Char): Map[Char, Seq[Pos]] = {
    val charToPositions = new mutable.HashMap[Char, ArrayBuffer[Pos]]
    grid.indices.foreach(y => grid(y).indices.foreach(x => {
      val c = grid(y)(x)
      if (except != c) {
        charToPositions.getOrElseUpdate(c, new ArrayBuffer[Pos]()) += Pos(y, x)
      }
    }))
    charToPositions.toMap.map((k, v) => k -> v.toSeq)
  }
  
  def forEachPos(consume: Pos => Unit): Unit = {
    grid.indices.foreach(y => grid(y).indices.foreach(x => consume(Pos(y, x))))
  }

  def positions: Seq[Pos] = grid.indices.flatMap(y => grid(y).indices.map(x => Pos(y, x)))

  def find(c: Char): Pos = {
    for {
      y <- grid.indices
      x <- grid(y).indices
      if grid(y)(x) == c
    } yield Pos(y, x)
  }.head
  
  def set(p: Pos, v: Char): Unit = grid(p.y) = grid(p.y).substring(0, p.x) + v + grid(p.y).substring(p.x + 1)

  def swap(pos1: Pos, pos2: Pos): Unit = {
    val v1 = grid(pos1)
    set(pos1, grid(pos2))
    set(pos2, v1)
  }
  
  def print(): Unit = {
    grid.foreach(line => println(line))
    println()
  }

  def prettyString(): String = {
    grid.mkString("\n")
  }
  
}

object ReachGridStr:
  def makeGrid(maxY: Int, maxX: Int, fill: Map[Char, Seq[Pos]], default: Char = '.'): Array[String] = {
    val grid: Array[String] = Array.fill(maxY)(Array.fill(maxX)(default).mkString)
    fill.foreach { case (c, poses) => poses.foreach(p => grid.set(p, c)) }
    grid
  }
end ReachGridStr