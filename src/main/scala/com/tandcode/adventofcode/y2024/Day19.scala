package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.Direction.*
import com.tandcode.adventofcode.api.{Direction, Pos, ReachGridStr, ReachString}

import scala.annotation.tailrec
import scala.collection.mutable

object Day19 {

  def part1(input: String): Long = {
    val Array(rTowels, rDesigns) = input.parts()
    val towels = rTowels.split(", ")
    val charToStrings = towels.groupBy(_.head)
    val startToTow = charToStrings.map((k, v) => k -> v.sorted(Ordering.by[String, Int](_.length).reverse))
    val designs = rDesigns.slines
    val wrongCache = new mutable.HashSet[String]()

    val strings: Array[String] = designs.filter(d => {

      def inner(des: String, i: Int): Boolean = {
        if i >= des.length then return true
        if wrongCache(des.substring(i)) then return false
        val arr = startToTow.getOrElse(des(i), Array.empty[String])
        val exists = arr.filter(tow => des.startsWith(tow, i)).exists(tow => inner(des, i + tow.length))
        if !exists then wrongCache += des.substring(i)
        exists
      }

      inner(d, 0)
    })
    strings.length
  }

  def part2(input: String): Long = {
    val Array(rTowels, rDesigns) = input.parts()
    val towels = rTowels.split(", ")
    val charToStrings = towels.groupBy(_.head)
    val startToTow = charToStrings.map((k, v) => k -> v.sorted(Ordering.by[String, Int](_.length).reverse))
    val designs = rDesigns.slines
    val wrongCache = new mutable.HashSet[String]()
    val correctCache = new mutable.HashMap[String, Int]()
    2
  }

}
