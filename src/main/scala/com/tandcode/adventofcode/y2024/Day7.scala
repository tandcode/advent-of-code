package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.Direction.*
import com.tandcode.adventofcode.api.{Direction, ReachString}


object Day7 {


  def combinations[T](list: Seq[T], n: Int): Seq[Seq[T]] = {
    if (n == 0) Seq(Nil) // Base case: if n is 0, return an empty combination
    else for {
      head <- list // Take each element as the "head"
      tail <- combinations(list, n - 1) // Recursively compute combinations for the rest
    } yield head +: tail // Combine the head with the rest
  }

  def part1(input: String): Long = {
    calcWithOps(input, Seq(_ + _, _ * _))
  }
  
  def part2(input: String): Long = {
    calcWithOps(input, Seq(_ + _, _ * _, (a, b) => (a.toString + b.toString).toLong))
  }

  def calcWithOps(input: String, ops: Seq[(Long, Long) => Long]) = {
    input.slines
      .map(line => {
        val Array(res, nums) = line.split(": ")
        (res.toLong, nums.split(" ").map(_.toLong))
      })
      .filter { case (res, nums) => combinations(ops, nums.length - 1)
        .exists(funcs => nums.tail.foldLeft((funcs, nums.head))
          { case ((leftFuns, result), num) => (leftFuns.tail, leftFuns.head(result, num)) }._2 == res)
      }
      .map(_._1)
      .sum
  }

}
