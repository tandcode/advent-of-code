package com.tandcode.adventofcode.y2024

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day1 {

  def part1(input: String): Int = {
    val l1 = new ArrayBuffer[Int]()
    val l2 = new ArrayBuffer[Int]()
    input
      .split("\n")
      .foreach(line => {
        val Array(n1, n2) = line.split("\\s+")
        l1.append(n1.toInt)
        l2.append(n2.toInt)
      })

    l1.sorted.zip(l2.sorted)
      .map((a, b) => math.abs(a - b))
      .sum
  }

  def part2(input: String): Int = {
    val l = new ArrayBuffer[Int]()
    val m = new mutable.HashMap[Int, Int]()
    input
      .split("\n")
      .foreach(line => {
        val Array(n1, n2) = line.split("\\s+")
        l.append(n1.toInt)
        val mKey = n2.toInt
        m.update(mKey, m.getOrElse(mKey, 0) + 1)
      })

    l.map(n => n * m.getOrElse(n, 0)).sum
  }

}
