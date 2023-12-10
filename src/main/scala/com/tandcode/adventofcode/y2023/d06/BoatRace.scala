package com.tandcode.adventofcode.y2023.d06

import scala.io.Source
import scala.util.Using

object BoatRace extends App {

  def timeToDistanceTestcases(input: String, merge: Boolean = false) = {
    val lines = input.split("(\\r\\n)")

    val Array(time, distance) = lines.map(line => {
      val numbers = line.substring(line.indexOf(":") + 1).trim.split("\\s+")
      (if merge then Seq(numbers.mkString) else numbers.toSeq).map(_.toLong)
    })

    time.zip(distance)
  }

  def howManyRaceCombinations(testCases: Seq[(Long, Long)]): Long = {
    testCases
      .map { case (time, dist) =>
        val times = 1L to time
        {
          for {
            v1 <- times.find(t => travelDistance(time, t) > dist)
            v2 <- times.findLast(t => travelDistance(time, t) > dist)
          } yield v2 - v1 + 1
        }.getOrElse(0L)
      }
      .product
  }

  def travelDistance(travelTime: Long, accelerationTime: Long) = accelerationTime * (travelTime - accelerationTime)

  val testValues =
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin

  val inputValues: String = Using(Source.fromResource("y2023/d06/input.txt"))(_.mkString).getOrElse("")

  println(s"Test fastest way, part 1: ${howManyRaceCombinations(timeToDistanceTestcases(testValues))}")
  println(s"Input fastest way, part 1: ${howManyRaceCombinations(timeToDistanceTestcases(inputValues))}")

  println(s"Test fastest way, part 2: ${howManyRaceCombinations(timeToDistanceTestcases(testValues, true))}")
  println(s"Input fastest way, part 2: ${howManyRaceCombinations(timeToDistanceTestcases(inputValues, true))}")
}
