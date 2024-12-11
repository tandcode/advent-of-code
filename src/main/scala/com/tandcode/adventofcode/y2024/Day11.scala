package com.tandcode.adventofcode.y2024

import scala.collection.mutable

object Day11 {

  def part1(input: String): String = countStones(input, 25).toString

  def part2(input: String): String = countStones(input, 75).toString

  private def countStones(input: String, depth: Int): BigInt = {
    val numbers = input.split(" ")
    val cache = new mutable.HashMap[(Int, String), BigInt]()
    numbers.map(n => countAtDepth(depth, n, cache)).sum
  }

  private def countAtDepth(depth: Int, n: String, cache: mutable.Map[(Int, String), BigInt]): BigInt = {
    if depth == 0 then return 1
    if cache.contains((depth, n)) then return cache((depth, n))
    nextNums(n).map(num => {
      val res = countAtDepth(depth - 1, num, cache)
      cache.put((depth - 1, num), res)
      res
    }).sum
  }

  private def nextNums(num: String): Seq[String] = num match
    case "0" => Seq("1")
    case num: String if num.length % 2 == 0 =>
      val second = num.substring(num.length / 2).dropWhile(c => c == '0')
      Seq(num.substring(0, num.length / 2), if second.isEmpty then "0" else second)
    case num: String => Seq((num.toLong * 2024).toString)

}