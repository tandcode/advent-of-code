package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.ReachString

import scala.annotation.tailrec
import scala.collection.mutable


object Day23 {

  def part1(input: String): Long = {
    val connections = calcConnections(input)
    connections
      .filter((c, cc) => c.startsWith("t"))
      .flatMap((c, cc) => cc
        .flatMap(c1 => connections(c1)
          .filter(c2 => connections(c2).contains(c)).map(c2 => Set(c1, c2)))
        .map(set => set + c))
      .toSet.size
  }

  def part2(input: String): String = {
    val connections = calcConnections(input)
    val maxPart = connections.foldLeft(Set.empty[String]) { case (maxParty, (c, cc)) =>
      @tailrec
      def findParty(start: String, queue: List[List[String]], maxSet: Set[String],
                    visited: mutable.Set[String] = new mutable.HashSet[String]()): Set[String] = {
        queue match
          case Nil => maxSet
          case con :: next =>
            val head = con.head
            val (nextQueue, nextMax) = if visited(head) then (next, maxSet) else {
              visited += head
              val headConnections = connections(head)
              val set = con.filter(headConnections).toSet + head
              if set.size < con.size then (next, maxSet) else {
                val newMax = if set.size > maxSet.size then set else maxSet
                val newCons = headConnections.filterNot(set).map(c => c :: con).toList
                val nextQueue = newCons ::: next
                (nextQueue, newMax)
              }
            }
            findParty(start, nextQueue, nextMax, visited)
      }

      val party = findParty(c, List(List(c)), Set.empty[String])
      if party.size > maxParty.size then party else maxParty
    }
    maxPart.toSeq.sorted.mkString(",")
  }

  private def calcConnections(input: String): mutable.HashMap[String, mutable.Set[String]] = {
    input.slines.foldLeft(new mutable.HashMap[String, mutable.Set[String]]()) { case (map, line) =>
      val Array(c1, c2) = line.split("-")
      map.getOrElseUpdate(c1, new mutable.HashSet[String]()) += c2
      map.getOrElseUpdate(c2, new mutable.HashSet[String]()) += c1
      map
    }
  }

}
