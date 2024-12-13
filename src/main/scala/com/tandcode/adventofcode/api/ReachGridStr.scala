package com.tandcode.adventofcode.api

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

implicit class ReachGridStr(grid: Array[String]) {
  
  def apply(pos: Pos): Char = grid(pos.y)(pos.x)

  def apply(pos: HeadingPos): Char = apply(pos.pos)
  
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
  
}