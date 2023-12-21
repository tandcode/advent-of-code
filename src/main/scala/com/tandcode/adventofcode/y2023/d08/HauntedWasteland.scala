package com.tandcode.adventofcode.y2023.d08

import com.tandcode.adventofcode.y2023.io.Util

import scala.annotation.tailrec



object HauntedWasteland {

  private def instructionsToTranslations(input: String) = {
    val Seq(instructionsStr, translations: _*) = Util.strToLines(input): @unchecked

    val keyToLeftRight = translations.map(line => {
      val key = line.substring(0, 3)
      val left = line.substring(7, 10)
      val right = line.substring(12, 15)
      key -> (left, right)
    }).toMap

    val instructions = instructionsStr.toIndexedSeq
    (instructions, keyToLeftRight)
  }

  private def inputToTotalStepsForStarts(input: String,
                                         startCondition: String => Boolean,
                                         endCondition: String => Boolean) = {
    val (instructions, keyToLeftRight) = instructionsToTranslations(input)
    val starts = keyToLeftRight.keySet.toList
      .filter(startCondition)

    @tailrec
    def stepsFromTo(key: String, keyToStop: String => Boolean, stepId: Long): Long = {
      if (keyToStop(key)) {
        stepId
      } else {
        val (left, right) = keyToLeftRight(key)
        val insctr = instructions((stepId % instructions.size.toLong).toInt)
        val nextKey = if insctr == 'L' then left else right
        stepsFromTo(nextKey, keyToStop, stepId + 1)
      }
    }

    starts.map(s => stepsFromTo(s, endCondition, 0))
  }

  def part1(input: String): Long =
    inputToTotalStepsForStarts(input, _ == "AAA", _ == "ZZZ")
      .head

  def part2(input: String): Long =
    inputToTotalStepsForStarts(input, _(2) == 'A', _(2) == 'Z')
      .reduce(lcm)

  private def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)

  private def lcm(a: Long, b: Long): Long = a * (b / gcd(a, b))
}
