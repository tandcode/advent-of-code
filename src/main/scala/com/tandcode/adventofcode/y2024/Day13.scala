package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.ReachString

object Day13 {


  def part1(input: String): Long = countTokens(input, identity)

  def part2(input: String): Long = countTokens(input, _ + 10_000_000_000_000L)

  /**
   * the key to solution is math, 2 equations with 2 unknowns
   * {{{
   *   equations:
   *   ax * a + bx * b = x
   *   ay * a + by * b = y
   *
   *   solution:
   *   a = (by * x - bx * y) / (ax * by - ay * bx)
   *   b = (ay * x - ax * y) / (ay * bx - ax * by)
   * }}}
   */
  def countTokens(input: String, mapping: Double => Double): Long = {
    val number = "\\d+".r
    val inputs = input.parts()
    inputs.map(inp => {
        val Array(Seq(ax, ay), Seq(bx, by), Seq(xInit, yInit)) = inp.slines
          .map(line => number.findAllMatchIn(line).map(_.group(0).toDouble).toSeq)
        val x = mapping(xInit)
        val y = mapping(yInit)
        val a = (by * x - bx * y) / (ax * by - ay * bx)
        val b = (ay * x - ax * y) / (ay * bx - ax * by)
        b + a * 3
      })
      .filter(r => r.toLong == r)
      .sum.toLong
  }

}