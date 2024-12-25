package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.Direction.*
import com.tandcode.adventofcode.api.{Direction, Pos, ReachGridStr, ReachString}
import com.tandcode.adventofcode.y2024.Day16.findShortestPath

import scala.annotation.tailrec
import scala.collection.mutable

object Day21 {
  val numpud = makeNumpud(
    3, Seq(
      '7', '8', '9',
      '4', '5', '6',
      '1', '2', '3',
      '.', '0', 'A'
    ))
  val arrowpud = makeNumpud(
    3, Seq(
      '.', '^', 'A',
      '<', 'v', '>'
    ))
  val cache = new mutable.TreeMap[Int, mutable.TreeMap[String, String]]()(Ordering.Int.reverse)

  def containsCache(key: String) = cache.get(key.length).exists(_.contains(key))
  def getCache(key: String) = cache(key.length)(key)
  def putCache(key: String, value: String, len: Int = 100) = {
    if key.length <= len then cache
      .getOrElseUpdate(key.length, new mutable.TreeMap[String, String]())
      .put(key, value)
  }
  def findCache(superKey: String): Option[(String, String)] = {
    var keyV: Option[(String, String)] = None

    cache.find(m => {
      val keyValue = m._2.find(kv => superKey.startsWith(kv._1))
      if keyValue.nonEmpty then keyV = keyValue
      keyValue.nonEmpty
    })
    keyV
  }

  def part1(input: String): Long = calculateWithIterations(input, 2)

  private def calculateWithIterations(input: String, iterations: Int): Long = {
    val lines = input.slines

    lines.map(line => {
      val next1 = calcNext(line, numpud)
      val next3 = directions(next1, iterations)
      val rr = next3.length
      val num = line.substring(0, line.length - 1).toInt
      rr * num
    }).sum
  }

  private def directions(cur: String, i: Int): String = {
    if i == 0 then cur else {
      val next = calcNext(cur, arrowpud)
      directions(next, i - 1)
    }
  }

  private def calcNext(chars: String, buttons: Map[Char, Pos]): String = {
    implicit val fartherOrd: Ordering[Char] = (a, b) => {
      if a == b then 0 else {
        val aNearA = a == '^' || a == '>'
        val bNearA = b == '^' || b == '>'
        if aNearA == bNearA then 0
        else if aNearA then -1
        else if bNearA then 1
        else if a == 'v' then -1
        else 1
      }
    }
    var result = ""
    val toIter = 'A' +: chars
    var i = 0
    while (i < toIter.length - 1) {
      val substr = toIter.substring(i)
      val value: Option[(String, String)] = findCache(substr)
      val (res, di) = value match
        case Some((key, value)) => (value, key.length - 1)
        case None => {
          val curP = buttons(toIter(i))
          val nextP = buttons(toIter(i + 1))

          val dY = nextP.y - curP.y
          val dX = nextP.x - curP.x
          //        val isYFirst = buttons('.') != curP.copy(y = curP.y + dY)
          val isDown = dY > 0
          val isRight = dX > 0
          val isYFirst = (isRight || isDown) && buttons('.') != curP.copy(y = curP.y + dY)
          val yMove = Seq.fill(math.abs(dY))(if isDown then 'v' else '^')
          val xMove = Seq.fill(math.abs(dX))(if isRight then '>' else '<')
          var steps = (yMove ++ xMove).sorted(fartherOrd.reverse)
          //        val steps = (if isYFirst then yMove ++ xMove else xMove ++ yMove)
          val poses = steps.scanLeft(curP) { case (p, d) => p(Direction(d)) }.tail
          val wrongI = poses.indexWhere(_ == buttons('.'))
          steps = if wrongI < 0 then steps else steps.reverse
          val res = (steps :+ 'A').mkString
          (res, 1)
        }

      result = result + res
      i += di
      val str = toIter.substring(0, i + 1)
      putCache(str, result)
    }
    result
  }

  private def makeNumpud(lineLen: Int, buttons: Seq[Char]): Map[Char, Pos] = {
    val lines = buttons.size / lineLen
    (for {
      y <- 0 until lines
      x <- 0 until lineLen
    } yield buttons(y * lineLen + x) -> Pos(y, x)).toMap
  }

  def part2(input: String): Long = calculateWithIterations(input, 25)

}
