package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.{Direction, HeadingPos, Pos, ReachGridStr, ReachString}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day12 {

  def part1(input: String): Long = {
    val grid: Array[String] = input.slines
    val groups = collectGroups(grid)

    groups
      .map(_._2.map(gr => {
          val set = gr.toSet
          val perim = gr.map(p => p.steps.filterNot(set).size).sum
          gr.size * perim
        }).sum
      ).sum
  }

  def part2(input: String): Long = {
    val grid: Array[String] = input.slines
    val positions = grid.positions
    val charToBuffer = new mutable.HashMap[Char, ArrayBuffer[ArrayBuffer[Pos]]]
    val groups = collectGroups(grid)

    groups
      .map(groups => {
        groups._2.map(gr => {
          val visited = new mutable.HashSet[HeadingPos]()
          val set = gr.toSet

          val perim = gr.map(p => {
            var res = 0
            val start = HeadingPos(p, Direction.Right)
            val left = start.turnLeft.next
            if (!visited(start) && !set(left.pos)) {
              @tailrec
              def countSides(pos: HeadingPos, sides: Int): Int = {
                if sides != 0 && pos == start then return sides
                val left = pos.turnLeft.next
                val next = pos.next
                visited += pos
                (set(left.pos), set(next.pos)) match
                  case (false, false) => countSides(pos.turnRight, sides + 1)
                  case (true, true) => countSides(pos.turnLeft.next, sides + 1)
                  case (false, true) => countSides(pos.next, sides)
                  case (true, false) => countSides(pos.turnLeft.next, sides + 1)
              }

              res = countSides(start, 0)
            }
            res
          }).sum

          gr.size * perim
        }).sum
      }).sum
  }

  private def collectGroups(grid: Array[String]): mutable.HashMap[Char, ArrayBuffer[ArrayBuffer[Pos]]] = {
    val positions = grid.positions
    val charToBuffer = new mutable.HashMap[Char, ArrayBuffer[ArrayBuffer[Pos]]]
    val (groups, _): (mutable.HashMap[Char, ArrayBuffer[ArrayBuffer[Pos]]], mutable.Set[Pos]) = positions
      .foldLeft((charToBuffer, mutable.HashSet[Pos]())) { case ((plantToRegions, visited), pos) =>
        if (!visited(pos)) {
          val group = new ArrayBuffer[Pos]()

          def eat(pos: Pos): Unit = {
            if (!visited(pos)) {
              group += pos
              visited += pos
            }
            val sameNext = pos.validSteps(grid)
              .filterNot(visited)
              .filter(next => grid(pos) == grid(next))
            sameNext.foreach(eat)
          }

          eat(pos)
          plantToRegions.getOrElseUpdate(grid(pos), new ArrayBuffer[ArrayBuffer[Pos]]()) += group
        }
        (plantToRegions, visited)
      }
    groups
  }

}