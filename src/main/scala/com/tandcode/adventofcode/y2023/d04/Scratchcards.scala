package com.tandcode.adventofcode.y2023.d04

import com.tandcode.adventofcode.io.Util.strToLines

object Scratchcards {

  def part1(input: String): Int = countWinPoints(strToLines(input))

  def part2(input: String): Int = countScratchcards(strToLines(input))

  val numsRex = "\\s*Card\\s*\\d+:\\s*([\\d\\s]+)\\|([\\d\\s]+)".r

  case class Counter(wins: Int) {
    private var count: Int = 1
    def inc(i: Int): Unit = count += i

    def total = count
  }
  def countWinPoints(input: Seq[String]) = input
    .map(line => {
      val numsRex(winNumbersStr, actualNumbersStr) = line: @unchecked
      val winNumbers = winNumbersStr.trim.split("\\s+").toSet
      val actualNumbers = actualNumbersStr.trim.split("\\s+").toSet
      val matches = winNumbers.intersect(actualNumbers).size

      if matches > 0 then 1 << (matches - 1) else 0
    }).sum

  def countScratchcards(input: Seq[String]) = {
    val winCards = input
      .map(line => {
        val numsRex(winNumbersStr, actualNumbersStr) = line: @unchecked
        val winNumbers = winNumbersStr.trim.split("\\s+").toSet
        val actualNumbers = actualNumbersStr.trim.split("\\s+").toSet
        val matches = winNumbers.intersect(actualNumbers).size

        winNumbers.intersect(actualNumbers).size
      })
      .map { win => Counter(win) }
      .toArray

    for { i <- winCards.indices } {
      val card = winCards(i)
      (0 until card.wins).foreach(v => winCards(v + 1 + i).inc(card.total))
    }

    winCards.map(_.total).sum
  }
}
