package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.Direction.*
import com.tandcode.adventofcode.api.{Direction, Pos, ReachGridStr, ReachString}
import com.tandcode.adventofcode.y2024.Day16.findShortestPath

import scala.annotation.tailrec
import scala.collection.mutable

object Day20 {

  def part1(input: String): Long = {
    val grid = input.slines
    val start = grid.positionOf('S')
    val end = grid.positionOf('E')

    val weights = findShortestPath(List((start, Direction.Up, 0)), end, grid)

    weights.map((p, w) => {
      Direction.values.toList
        .count(d => {
          val next = p(d)
          val nextNext = next(d)
          val isCheat = grid.matches(next)('#') && grid.matches(nextNext)('.', 'E')
          val nnWeight = weights.getOrElse(nextNext, 0)
          val diff = nnWeight - (w + 2)
          isCheat && diff >= 100
        })
    }).sum

  }

  def part2(input: String): Long = {
    val Array(rTowels, rDesigns) = input.parts()
    val towels = rTowels.split(", ")
    val charToStrings = towels.groupBy(_.head)
    val startToTow = charToStrings.map((k, v) => k -> v.sorted(Ordering.by[String, Int](_.length).reverse))
    val designs = rDesigns.slines
    val wrongCache = new mutable.HashSet[String]()
    val correctCache = new mutable.HashMap[String, Int]()

    2
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

  private def findShortestPath(positions: List[(Pos, Direction, Int)], end: Pos,
                               grid: Array[String],
                               weights: mutable.Map[Pos, Int] = new mutable.HashMap[Pos, Int]()): Map[Pos, Int] = {
    positions match
      case Nil => weights.toMap
      case (pos, dir, score) :: tail =>
        val key = pos
        val hasKey = weights.contains(key)
        val doUpdateWeight = !hasKey || weights(key) > score
        if doUpdateWeight then weights(key) = score
        val newTail = if doUpdateWeight then
          val newSteps = Direction.values.toList
            .filter(d => score == 0 || d != dir.turnAround)
            .flatMap(d => {
              val next = pos(d)
              Option.when(!grid.matches(next)('#'))((next, d, score + 1))
            })
          newSteps ::: tail
        else
          tail
        findShortestPath(newTail, end, grid, weights)
  }

//  def countCheats(grid: Array[String], weights: Map[Pos, Int], origin: Pos, p: Pos, dir: Direction, curCheats: Int = 1, maxCheats: Int): Int = {
//    Direction.values
//      .filter(d => d != dir.turnAround)
//      .flatMap(d => {
//        val next = p(d)
//        val isValid = next.validForGrid(grid)
//      })
//    val steps = p.validSteps(grid)
//    if curCheats <= maxCheats then {
//      val i = steps.count(next => {
//        val isNextCheat = grid.matches(next)('.', 'E')
//        val nWeight = weights.getOrElse(next, 0)
//        val i1 = weights(origin) + curCheats
//        val diff = nWeight - i1
//        isNextCheat && diff >= 20
//      })
//      val sum = steps.filter(n => weights.contains(n)).map(n => countCheats(grid, weights, origin, n, d, curCheats + 1, maxCheats)).sum
//      i + sum
//    } else 0
//  }

}
