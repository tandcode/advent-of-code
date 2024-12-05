package com.tandcode.adventofcode.y2024

import scala.annotation.tailrec
import scala.collection.mutable

object Day4 {

  val xmas = "XMAS"
  val mas = "MAS"

  def part1(input: String): Long = {
    val lines = input.split('\n')

    lines.indices
      .flatMap(y => lines(y).indices
        .map(x => if lines(y)(x) == xmas(0)
        then Seq((0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1))
            .count { case (dy, dx) => equalsWord(lines, y, x, dy, dx, xmas) }
        else 0
        )
      )
      .sum
  }

  @tailrec
  def equalsWord(lines: Array[String], y: Int, x: Int, dy: Int, dx: Int, word: String, i: Int = 1): Boolean = {
    if i == word.length then return true
    val nextY = y + dy * i
    val nextX = x + dx * i
    if nextY < 0 || nextY >= lines.length || nextX < 0 || nextX >= lines(y).length then return false
    if lines(nextY)(nextX) == word(i) then equalsWord(lines, y, x, dy, dx, word, i + 1) else false
  }

  def part2(input: String): Long = {
    val lines = input.split('\n')

    lines.indices
      .flatMap(y => lines(y).indices
        .map(x => if lines(y)(x) == mas(0)
        then Seq((1, 1), (1, -1), (-1, 1), (-1, -1))
            .count { case (dy, dx) => equalsXDashMas(lines, y, x, dy, dx) }
        else 0
        )
      )
      .sum / 2
  }

  def equalsXDashMas(lines: Array[String], y: Int, x: Int, dy: Int, dx: Int): Boolean = {
    def incr(num: Int, d: Int): Int = if d > 0 then num + 2 else num - 2

    val hasWord = equalsWord(lines, y, x, dy, dx, mas)
    if !hasWord then return false
    val hasSecond1 = equalsWord(lines, incr(y, dy), x, -dy, dx, mas, 0)
    val hasSecond2 = equalsWord(lines, y, incr(x, dx), dy, -dx, mas, 0)
    hasSecond1 || hasSecond2
  }


}
