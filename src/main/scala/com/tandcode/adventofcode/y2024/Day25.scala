package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.{ReachGridStr, ReachString}

object Day25 {

  def part1(input: String): Long = {
    val (rawLocks, rawKeys) = input.parts().partition(in => in.startsWith("#"))
    val locks = rawLocks.map(read)
    val keys = rawKeys.map(read)
    val lockHeight = 7
    val diffs = for {
      lock <- locks
      key <- keys
    } yield lock.zip(key).map(_ + _)
    diffs.count(d => d.forall(p => p <= lockHeight))
  }

  def read(inp: String): Seq[Int] = {
    val grid = inp.slines
    grid.countColumns('#')
  }


  def part2(input: String): Long = 2024

}
