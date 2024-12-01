package com.tandcode.adventofcode.y2023.d14

import com.tandcode.adventofcode.y2023.d14.ParabolicReflector.Direction
import com.tandcode.adventofcode.y2023.d14.ParabolicReflector.Direction.{East, North, South, West}
import com.tandcode.adventofcode.io.Util.strToLines

import scala.annotation.tailrec
import scala.collection.immutable.Set
import scala.collection.mutable

object ParabolicReflector {

  enum Direction:
    case North, West, South, East

  def part1(input: String): Long = tilt(input, Seq(North), 1)

  def part2(input: String): Long = tilt(input, Direction.values, 1000000000)

  private def tilt(input: String, dirs: Seq[Direction], cycles: Int) = {
    val grid = strToLines(input)
      .map(_.toCharArray).toArray

    val gridSnapshots: mutable.Map[IndexedSeq[IndexedSeq[Char]], Int] = mutable.HashMap()

    @tailrec
    def loop(cycle: Int, cyclesMax: Int): Seq[Seq[Char]] = {
      val array = grid.map(_.toIndexedSeq).toIndexedSeq

      if cycle >= cyclesMax then array
      else {
        dirs.foreach(dir => {
          val (yIndices, xIndices) = dir match
            case North => (1 until grid.length) -> grid(0).indices
            case West => grid.indices -> (1 until grid(0).length)
            case South => (0 until grid.length - 1).reverse -> grid(0).indices
            case East => grid.indices -> (0 until grid(0).length - 1).reverse

          yIndices foreach (y =>
            xIndices foreach (x =>
              if grid(y)(x) == 'O' then rollTheStone(grid, y, x, dir)
              )
            )
        })

        if (gridSnapshots.contains(array)) {
          val loopStart = gridSnapshots(array)
          val loopEnd = cycle
          val step = loopEnd - loopStart
          val index = (cyclesMax - loopStart) % step + loopStart

          gridSnapshots.find(_._2 == index).map(_._1).getOrElse(array)
        } else {
          gridSnapshots.put(array, cycle)
          loop(cycle + 1, cyclesMax)
        }
      }
    }

    val gridAtCycle = loop(0, cycles)

    gridAtCycle.indices
      .map(i =>
        val weight = grid.length - i
        gridAtCycle(i).count(_ == 'O') * weight
      ).sum
  }

  private def rollTheStone(grid: Array[Array[Char]], y: Int, x: Int, dir: Direction): Unit = {
    val stopRolling = Set('#', 'O')

    @tailrec
    def rollVertical(yy: Int, isNorth: Boolean): Unit = {
      val isEdge = if isNorth then yy == 0 else yy == grid.length - 1
      val nextY = if isNorth then yy - 1 else yy + 1
      if isEdge || stopRolling(grid(nextY)(x)) then {
        grid(y)(x) = '.'
        grid(yy)(x) = 'O'
      }
      else rollVertical(nextY, isNorth)
    }

    @tailrec
    def rollWest(xx: Int, isWest: Boolean): Unit = {
      val isEdge = if isWest then xx == 0 else xx == grid(0).length - 1
      val nextX = if isWest then xx - 1 else xx + 1
      if isEdge || stopRolling(grid(y)(nextX)) then {
        grid(y)(x) = '.'
        grid(y)(xx) = 'O'
      }
      else rollWest(nextX, isWest)
    }

    dir match
      case North => rollVertical(y, true)
      case West => rollWest(x, true)
      case South => rollVertical(y, false)
      case East => rollWest(x, false)
  }

}
