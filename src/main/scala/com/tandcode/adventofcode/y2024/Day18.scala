package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.Direction.*
import com.tandcode.adventofcode.api.{Direction, Pos, ReachGridStr, ReachString}

import scala.annotation.tailrec
import scala.collection.mutable

object Day18 {

  def part1(input: String): Long = {
    val (obstacles, size, len) = readInput(input)
    val (grid, start, end) = gridStartEnd(obstacles, size, len)
    findShortestPath(start, end, 0, grid).get
  }

  def part2(input: String): String = {
    val (obstacles, size, len) = readInput(input)
    @tailrec
    def find(i: Int): Int = {
      val (grid, start, end) = gridStartEnd(obstacles, size, i)
      if findShortestPath(start, end, 0, grid).isEmpty then find(i - 1) else i
    }
    val pos = obstacles(find(obstacles.length))
    s"${pos.x},${pos.y}"
  }

  private def gridStartEnd(obstacles: Array[Pos], size: Int, len: Int): (Array[String], Pos, Pos) = {
    val grid = ReachGridStr.makeGrid(size, size, Map('#' -> obstacles.take(len).toSeq))
    val start = Pos(0, 0)
    val end = Pos(size - 1, size - 1)
    (grid, start, end)
  }

  private def readInput(input: String): (Array[Pos], Int, Int) = {
    val lines = input.slines
    val obstacles: Array[Pos] = lines.init.map(s => {
      val Array(x, y) = s.split(",").map(_.toInt)
      Pos(y, x)
    })
    val Array(size, len) = lines.last.split(" ").map(_.toInt)
    (obstacles, size, len)
  }

  private def findShortestPath(pos: Pos, end: Pos, score: Int,
                               grid: Array[String],
                               weights: mutable.HashMap[Pos, Int] = new mutable.HashMap[Pos, Int]()): Option[Int] = {
    if pos == end then return Some(score)
    if weights.contains(pos) then
      if weights(pos) > score then weights(pos) = score else return None
    else weights(pos) = score

    val steps = pos.validSteps(grid)
      .filter(p => grid(p) != '#')
      .flatMap(next => findShortestPath(next, end, score + 1, grid, weights))
    Option.when(steps.nonEmpty)(steps).map(_.min)
  }

}
