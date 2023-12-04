package com.tandcode.adventofcode.y2023.d04

import scala.io.Source
import scala.util.Using

object Scratchcards extends App {

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

  val testValues = Seq(
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  )

  val inputValues: Seq[String] = Using(Source.fromResource("y2023/d04/input.txt"))(_.getLines().toSeq).getOrElse(Nil)

  println(s"Test sum of win points: ${countWinPoints(testValues)}")
  println(s"Input sum of win points: ${countWinPoints(inputValues)}")

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

  println(s"Test sum of win points: ${countScratchcards(testValues)}")
  println(s"Input sum of win points: ${countScratchcards(inputValues)}")

}
