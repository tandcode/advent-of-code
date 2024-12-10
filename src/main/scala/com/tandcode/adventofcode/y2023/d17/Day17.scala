package com.tandcode.adventofcode.y2023.d17

import com.tandcode.adventofcode.api.{Direction, HeadingPos, Pos, ReachGridInt}
import com.tandcode.adventofcode.io.Log.withTimeLog
import com.tandcode.adventofcode.io.{ReachString, ReachStrings}

import scala.collection.mutable

object Day17 {

  def part1(input: String): Long = {
    withTimeLog {
      implicit val numbers: Array[Array[Int]] = input.slines.map(_.split("").numbers)

      val nodes = new SortedNodes(n => n.weight)
      nodes.add(Node(Pos(0, 0), Direction.Up, 0, 0, new mutable.LinkedHashSet()))

      val res = search(nodes, Pos(numbers.length - 1, numbers(0).length - 1), Int.MaxValue, 2, 0)
      res
    }
  }

  def part2(input: String): Long = {
    withTimeLog {
      implicit val numbers: Array[Array[Int]] = input.slines.map(_.split("").numbers)

      val nodes = new SortedNodes(n => n.weight)
      nodes.add(Node(Pos(0, 0), Direction.Right, 0, 0, new mutable.LinkedHashSet()))
      nodes.add(Node(Pos(0, 0), Direction.Down, 0, 0, new mutable.LinkedHashSet()))

      val res = search(nodes, Pos(numbers.length - 1, numbers(0).length - 1), Int.MaxValue, 10, 4)
      res
    }
  }

  def search[K](nodes: SortedNodes[K],
                target: Pos,
                minTargetWeight: Int,
                maxLen: Int,
                minLen: Int,
                minWaits: mutable.Map[(HeadingPos, Int), Int] = new mutable.HashMap[(HeadingPos, Int), Int]())
               (implicit grid: Array[Array[Int]]): Int = {

    val node = nodes.popMin
    if (node.weight >= minTargetWeight) {
      return minTargetWeight
    }
    val boo = node.weight == 102
    val isNode = node.current == Pos(10, 12)
    val moreNodes = nextNodes(node)(maxLen, minLen)
    val nodes1 = moreNodes
      .filterNot(nodes.hasNodeWithWeightAndPos)
    val nodes2 = nodes1
      .filter(n => {
        val len = n.numDirs
        val key = (n.toHeadingPos, len)
        val noMinWeight = !minWaits.get(key).exists(_ < n.weight)
        if noMinWeight then minWaits.put(key, n.weight)
        noMinWeight
      })
    nodes2
      .foreach(nodes.add)

    val nextTargetWeight = moreNodes
      .filter(n => n.current == target)
      .filter(n => n.weight < minTargetWeight)
      .map(_.weight)
      .headOption
      .getOrElse(minTargetWeight)

    search(nodes, target, nextTargetWeight, maxLen, minLen, minWaits)
  }

  def nextNodes(node: Node)(maxLen: Int, minLen: Int)(implicit grid: Array[Array[Int]]): Seq[Node] = {
    val isMaxLen = node.numDirs == maxLen
    val isMinLen = node.numDirs < minLen // 0 4
    Direction.values
      .filter(d => !isMaxLen || node.lastDir != d)
      .filter(d => !isMinLen || node.lastDir == d)
      .flatMap(dir => {
        val nextPos = node.current(dir)
        val isNotVisited = !node.visited(nextPos)
        if isNotVisited && nextPos.validForGrid(grid)
        then {
          val newWeight = node.weight + grid(nextPos)
          val newVisited = new mutable.LinkedHashSet[Pos]()
          newVisited ++= node.visited
          newVisited += node.current
          val nextNumDirs = if dir == node.lastDir then node.numDirs + 1 else 0
          Some(Node(nextPos, dir, nextNumDirs , newWeight, newVisited))
        }
        else None
      }).toSeq
  }

  case class Node(current: Pos,
                  lastDir: Direction,
                  numDirs: Int,
                  weight: Int = 0,
                  visited: mutable.Set[Pos])(
                   implicit grid: Array[Array[Int]]
                 ) {
    def toHeadingPos: HeadingPos = HeadingPos(current, lastDir)

    lazy val lengthToEnd: Double = {
      math.sqrt(math.pow(grid(0).length - current.x, 2) + math.pow(grid.length - current.y, 2))
    }
  }

  class SortedNodes[K: Ordering](keyFn: Node => K) {
    val map: mutable.TreeMap[K, mutable.ArrayDeque[Node]] = new mutable.TreeMap[K, mutable.ArrayDeque[Node]]()

    def add(node: Node): mutable.Seq[Node] = {
      map.getOrElseUpdate(keyFn(node), new mutable.ArrayDeque[Node]()) += node
    }

    def popMin: Node = {
      val head = map.head
      val nodes: mutable.ArrayDeque[Node] = head._2
      if (nodes.isEmpty) {
        map.remove(head._1)
        popMin
      } else {
        nodes.remove(0)
      }
    }

    def hasNodeWithWeightAndPos(n: Node): Boolean = {
      map.getOrElse(keyFn(n), Seq.empty)
        .exists(node => node.current == n.current && node.lastDir == n.lastDir && node.numDirs == n.numDirs)
    }
  }
}
