package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.Direction.*
import com.tandcode.adventofcode.api.{Direction, Pos, ReachGridStr, ReachString}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



object Day16 {

  def part1(input: String): Long = {
    val grid: Array[String] = input.slines
    val start: Pos = grid.positionOf('S')
    val end: Pos = grid.positionOf('E')
    findShortestPath(List((start, Direction.Right, 0)), end, grid)
  }

  def part2(input: String): Long = {
    val grid: Array[String] = input.slines
    val start: Pos = grid.positionOf('S')
    val end: Pos = grid.positionOf('E')
    findShortestPath2(List(Path((start, Direction.Right, 0), Nil)), end, grid)
  }


  @tailrec
  private def findShortestPath(positions: List[(Pos, Direction, Int)],
                               end: Pos,
                               grid: Array[String],
                               weights: mutable.HashMap[(Pos, Boolean), Int] = new mutable.HashMap[(Pos, Boolean), Int]()): Int = {
    positions match
      case Nil => List(weights.get((end, true)), weights.get((end, false))).flatten.min
      case (pos, dir, score) :: tail =>
        val key = (pos, dir.isHorizontal)
        val hasKey = weights.contains(key)
        val doUpdateWeight = !hasKey || weights(key) > score
        if doUpdateWeight then weights(key) = score
        val newTail = if doUpdateWeight then
          val newSteps = Direction.values.toList
            .filter(_ != dir.turnAround)
            .filterNot(d => grid.matches(pos(d))('#'))
            .map(d => (pos(d), d, score + (if dir == d then 1 else 1001)))
          newSteps ::: tail
        else
          tail
        findShortestPath(newTail, end, grid, weights)
  }

  private def findShortestPath2(positions: List[Path],
                                end: Pos,
                                grid: Array[String],
                                snakes: mutable.Map[Pos, ArrayBuffer[Path]] = new mutable.HashMap[Pos, ArrayBuffer[Path]](),
                                weights: mutable.HashMap[(Pos, Boolean), Int] = new mutable.HashMap[(Pos, Boolean), Int]()): Int = {
    positions match
      case Nil => {
        val shortest = snakes(end).minBy(_.head._3)
        @tailrec
        def loop(snake: List[Pos], res: Set[Pos]): Int = {
          snake match
            case Nil => {
              res.size
            }
            case head :: next => {
              val toAdd = if snakes.contains(head) then {

                Nil
              } else Nil
              loop(next, res ++ toAdd.toSet)
            }
        }

        val toAdd = loop(shortest.tail, Set.empty[Pos])
        val res = shortest.tail.size
        res + toAdd
      }
      case head :: tail =>
        val (pos, dir, score) = head.head
        val key = (pos, dir.isHorizontal)
        val hasKey = weights.contains(key)
        val doUpdateWeight = !hasKey || weights(key) > score
        if doUpdateWeight then weights(key) = score

        val newTail = if doUpdateWeight then
          val newSteps = Direction.values.toList
            .filter(_ != dir.turnAround)
            .filterNot(d => grid.matches(pos(d))('#'))
            .map(d => (pos(d), d, score + (if dir == d then 1 else 1001)))
            .map(h => Path(h, pos :: head.tail))
          if pos == end || newSteps.size > 1 || true then snakes.getOrElseUpdate(pos, new ArrayBuffer[Path]()).addOne(head)
          newSteps ::: tail
        else
          tail
        findShortestPath2(newTail, end, grid, snakes, weights)
  }

  def groupByTurn(snakes: ArrayBuffer[Path]) = {
    val paths = new mutable.HashMap[Int, ArrayBuffer[Path]]()
    snakes.foreach(p => {
      val weight = p.head._3
      if paths.contains(weight - 1000) then paths(weight - 1000).addOne(p)
      else if paths.contains(weight + 1000) then paths(weight + 1000).addOne(p)
      else paths.getOrElseUpdate(weight, new ArrayBuffer[Path]()).addOne(p)
    })
    paths.find((k,v) => v.size > 1).map(_._2).getOrElse(Nil)
  }

  case class Path(head: (Pos, Direction, Int), tail: List[Pos])

}

