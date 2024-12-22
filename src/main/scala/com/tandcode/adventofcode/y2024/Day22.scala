package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.Direction.*
import com.tandcode.adventofcode.api.{Direction, Pos, ReachGridStr, ReachString}
import com.tandcode.adventofcode.y2024.Day16.findShortestPath

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object Day22 {

  def part1(input: String): Long = {
    val slines = input.slines
    slines.map(n => {
      (0 until 2000).foldLeft(n.toLong) { case (sn, _) => nextSn(sn) }
    }).sum
  }

  private def nextSn(sn: Long): Long = {
    val sn1 = prune(mix(sn * 64, sn))
    val sn2 = prune(mix(sn1 / 32, sn1))
    prune(mix(sn2 * 2048, sn2))
  }

  def part2(input: String): Long = {
    val slines = input.slines
    val range = 0 to 2000
    val prices = slines.map(n => {
      range.foldLeft((new ListBuffer[Int], n.toLong)) { case ((res, sn), _) =>
        val nextSecNum = nextSn(sn)
        res += sn.toString.last.asDigit
        (res, nextSecNum)
      }._1
    })
    val diffs = prices.map(price => price.sliding(2).map { case(ListBuffer(pr, cur)) => cur - pr}.toArray)
    val valPerPrev4Val = diffs.indices.map(di => {
      val d = diffs(di)
      val slides = d.sliding(4, 1).toArray
      slides.indices.foldLeft(new mutable.HashMap[List[Int], Int]()) { case (map, i) =>
        val ints = slides(i).toList
        val maybeMax = prices(di)(i + 4)
        if !map.contains(ints) then map.put(ints, maybeMax)
        map
      }
    }).toList
    val all4ValKeys = valPerPrev4Val.foldLeft(Set.empty[List[Int]])((r, m) => r ++ m.keySet)
    val keyToSum = all4ValKeys.map(k => k -> valPerPrev4Val.map(_.getOrElse(k, 0)).sum)
    val (_, max) = keyToSum.maxBy(_._2)
    max
  }

  def prune(sn: Long): Long = sn % 16777216

  def mix(res: Long, sn: Long): Long = res ^ sn

}
