package com.tandcode.adventofcode.y2024

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



object Day2 {

  def part1(input: String): Int = {
    val booleans = input
      .split("\n")
      .map(line => isSafe(line.split("\\s+").map(_.toLong), 0, None))
    booleans.count(identity)
  }

  def isSafe(nums: Array[Long], i: Int = 0, isDecreasing: Option[Boolean] = None): Boolean = {
    if (i == nums.length - 1) {
      return true
    }
    val diff = nums(i) - nums(i + 1)
    val isDecr = diff > 0
    val absDiff = math.abs(diff)
    val isCurrentSafe = isDecreasing.forall(_ == isDecr) && absDiff > 0 && absDiff < 4
    if (!isCurrentSafe) {
      return false
    }
    isSafe(nums, i + 1, Some(isDecr))
  }

  def part2(input: String): Int = {
    val booleans = input
      .split("\n")
      .map(line => isSafeP2(line.split("\\s+").map(_.toLong)))
    booleans.count(identity)
  }

  def isSafeP2(nums: Array[Long]): Boolean = {
    if isSafe(nums) then true else {
      nums.indices.exists(i => isSafe(nums.splitAt(i) match { case (first, second) => first ++ second.tail }))
    }
  }

  // todo: maybe revise
  def isSafeNotWorking(nums: Array[Long], i: Int = 0, isDecreasing: Option[Boolean] = None, allowFails: Int = 0): Boolean = {
    def checkWithIndexes(i1: Int, i2: Int, isPrevDecr: Option[Boolean]): Option[(Boolean, Option[Boolean])] = {
      if (i2 >= nums.length) {
        return None
      }
      if (i1 < 0) {
        return Some(true, isPrevDecr)
      }
      val diff = nums(i1) - nums(i2)
      val isDecrease = diff > 0
      val absDiff = math.abs(diff)
      val isSafe = isPrevDecr.forall(_ == isDecrease) && absDiff > 0 && absDiff < 4
      Some(isSafe, Some(isDecrease))
    }

    checkWithIndexes(i, i + 1, isDecreasing) match
      case None => true
      case Some((isCurrSafe, isCurrDecr)) =>
        var nextAllowFailures = allowFails
        if (!isCurrSafe) {
          nextAllowFailures -= 1
          val res: Option[Boolean] = checkWithIndexes(i, i + 2, isCurrDecr) match
            case None => Some(true)
            case Some((isNextSafe, _)) =>
              checkWithIndexes(i - 1, i + 1, isCurrDecr) match
                case None => Some(true)
                case Some((isPrevSafe, _)) =>
                  if (!(isNextSafe || isPrevSafe)) {
                    nextAllowFailures -= 1
                  }
                  None
          if (nextAllowFailures < 0) {
            return false
          }
          return res.getOrElse(true)
        }
        val nextI = if isCurrSafe then i + 1 else i + 2
        isSafeNotWorking(nums, nextI, isCurrDecr, nextAllowFailures)
  }
}
