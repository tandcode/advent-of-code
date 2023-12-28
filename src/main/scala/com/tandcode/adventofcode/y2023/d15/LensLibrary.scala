package com.tandcode.adventofcode.y2023.d15

import scala.annotation.tailrec
import scala.collection.mutable

object LensLibrary {

  def part1(input: String): Long = {
    input
      .split(",")
      .map(str => calcHash(str.toCharArray))
      .sum
  }

  @tailrec
  def calcHash(str: Array[Char], i: Int = 0, hash: Int = 0): Int =
    if i >= str.length then hash
    else calcHash(str, i + 1, ((hash + str(i)) * 17) % 256)

  def part2(input: String): Long = {
    val arr = (0 to 256).map(_ => new mutable.LinkedHashMap[String, Int]()).toArray
    input
      .split(",")
      .foreach(str => {
        val strings = str.split("[-=]")
        val label = strings.head
        val box = calcHash(label.toCharArray)
        if (strings.length == 1) {
          arr(box).remove(label)
        } else {
          arr(box).put(label, strings.last.toInt)
        }
      })

    arr.zipWithIndex
      .map { case (lenses, boxI) =>
        lenses.values.zipWithIndex.map { case (focalLength, slotI) => focalLength * (slotI + 1) }.sum * (boxI + 1)
      }.sum
  }
}
