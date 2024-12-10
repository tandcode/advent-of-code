package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.io.Log.withTimeLog
import com.tandcode.adventofcode.io.{Log, Util}
import com.tandcode.adventofcode.y2024.Day6.Direction

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.control.Breaks.{break, breakable}

object Day9 {

  def part1(input: String): String = {
    val (arr, _) = input.foldLeft((new ListBuffer[String](), 0)) { case ((res, i), next) =>
      (0 until next.asDigit).foreach(_ => res += (if i % 2 == 0 then (i / 2).toString else "."))
      (res, i + 1)
    }
    val inArr = arr.toArray
    var i = 0
    var j = inArr.length - 1
    while (i != j) {
      if (inArr(i) == ".") {
        if (inArr(j) != ".") {
          inArr(i) = inArr(j)
          inArr(j) = "."
          i += 1
        }
        j -= 1
      } else {
        i += 1
      }
    }
    val sum = inArr.takeWhile(_ != ".").indices.map(i => BigInt(i) * BigInt(inArr(i).toInt)).sum
    sum.toString
  }

  // unfinished
  def part2(input: String): String = {
    val (inArr, _) = input.foldLeft((new ArrayBuffer[String](), 0)) { case ((res, i), next) =>
      (0 until next.asDigit).foreach(_ => res += (if i % 2 == 0 then (i / 2).toString else "."))
      (res, i + 1)
    }

    var i = 0
    var j = inArr.length - 1
    while (i < j) {
      if (inArr(i) == ".") {
        if (inArr(j) != ".") {
          inArr(i) = inArr(j)
          inArr(j) = "."
          i += 1
        }
        j -= 1
      } else {
        i += 1
      }
    }
    val sum = inArr.takeWhile(_ != ".").indices.map(i => BigInt(i) * BigInt(inArr(i).toInt)).sum
    sum.toString
  }

  def part1Gold(input: String): String = {
    val (data, blanks) = input.indices.toArray.partition(_ % 2 == 0)
    val numbers: Array[Int] = input.map(_.asDigit).toArray
    val dataIter = data.indices.iterator
    val blanksIter = blanks.indices.iterator
    var res: Long = 0L
    var i = 0
    val bdIter = new BackIter(data, numbers)
    breakable {
      while (bdIter.hasNext()) {
        val dNext = dataIter.nextOption()
        val bNext = blanksIter.nextOption()
        dNext.foreach(evenI => {
          val num = numbers(data(evenI))
          (0 until num).foreach(h => {
            val add = evenI * i
            res = res + add
            val (ii, jj) = bdIter.getNextIndex()
            if (ii == evenI) {
              if (h == num - jj) {
                break()
              }
            }
            i += 1
          })
        })

        bNext.foreach(unevenI => {
          (0 until numbers(blanks(unevenI))).foreach(_ => {
            if (bdIter.hasNext()) {
              val (ii, jj) = bdIter.getNextIndex()
              if (ii == dNext.get) {
                break()
              }
              val curV = bdIter.next()
              val add = curV * i
              res = res + add
              i += 1
            }
          })
        })
      }
    }

    res.toString
  }

  // unfinished
  def part2Gold(input: String): String = {
    val (data, blanks) = input.indices.toArray.partition(_ % 2 == 0)
    val numbers: Array[Int] = input.map(_.asDigit).toArray
    val dataIter = data.indices.iterator
    val blanksIter = blanks.indices.iterator
    var res: Long = 0L
    var i = 0
    val bdIter = new BackIter(data, numbers)
    val usedBlanksIns = new mutable.HashSet[Int]()

    breakable {
      while (bdIter.hasNext()) {
        val dNext = dataIter.nextOption()
        val bNext = blanksIter.nextOption()
        dNext.foreach(evenI => {
          val num = numbers(data(evenI))
          (0 until num).foreach(n => {
            val add = evenI * i
            res = res + add
            val (ii, jj) = bdIter.getNextIndex()
            if (ii == evenI) {
              val diff = num - jj
              if (n == diff) {
                break()
              }
            }
            i += 1
          })
        })

        val (len, ii) = bdIter.nextBlock()
        val blanI = blanks.indices.find(in => {
          val num = numbers(blanks(in))
          num <= len
        })

        bNext.foreach(unevenI => {

          (0 until numbers(blanks(unevenI))).foreach(_ => {
            if (bdIter.hasNext()) {
              val (ii, jj) = bdIter.getNextIndex()
              if (ii == dNext.get) {
                break()
              }
              val curV = bdIter.next()
              val add = curV * i
              res = res + add
              i += 1
            }
          })
        })
      }
    }

    res.toString
  }


  class BackIter(data: Array[Int], input: Array[Int]) {
    private var i: Int = data.length - 1
    private var j: Int = 1

    def hasNext(): Boolean = {
      i >= 0
    }

    def next(): Int = {
      val resI = i
      if j < input(data(i)) then {
        j += 1
      } else {
        i -= 1
        j = 1
      }
      resI
    }

    def nextBlock(): (Int, Int) = {
      val resI = i
      i -= 1
      (input(data(resI)), resI)
    }

    def getNextIndex(): (Int, Int) = {
      (i, j)
    }
  }


}
