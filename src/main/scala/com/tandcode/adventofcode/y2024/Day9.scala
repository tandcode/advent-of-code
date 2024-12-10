package com.tandcode.adventofcode.y2024

import scala.annotation.tailrec
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
    val data = mutable.ListBuffer(input.indices.filter(_ % 2 == 0).map(i => input(i).asDigit):_*)
    val blanks = mutable.ListBuffer(input.indices.filter(_ % 2 == 1).map(i => input(i).asDigit):_*)

    var bi = 0
    var di1 = 0
    var di2 = data.length - 1
    val sb = new mutable.ArrayBuffer[String]()
    val visitedNums = new mutable.HashSet[Int]()
    val widthToOrderedData: mutable.TreeMap[Int, ArrayBuffer[Int]] = new mutable.TreeMap[Int, mutable.ArrayBuffer[Int]]()
    data.indices.foreach(ii => {
      val value = data(ii)
      widthToOrderedData.getOrElseUpdate(value, new ArrayBuffer[Int]()) += ii
    })

    while (di1 < data.length) {
      if (visitedNums(di1)) {
        (0 until data(di1)).foreach(di => sb.append("."))
      } else {
        (0 until data(di1)).foreach(di => sb.append(di1.toString))
        val ints = widthToOrderedData(data(di1))
        val ind = ints.indexOf(di1)
        if ints.size == 1 then widthToOrderedData.remove(data(di1)) else ints.remove(ind)
      }
      di1 += 1
      val nextBlankSize = if bi < blanks.size then Option(blanks(bi)) else None
      nextBlankSize.foreach(nbs => {
        @tailrec
        def inner(nbss: Int, res: Seq[String]): (Int, Seq[String]) = {
          val matchingData = widthToOrderedData.keys.filter(k => k <= nbss)
          if matchingData.isEmpty then return (nbss, Nil)
          val maxK = matchingData.maxBy(k => widthToOrderedData(k).last)
          val maxList = widthToOrderedData(maxK)
          val maxI = maxList.last
          visitedNums += maxI
          if maxList.size == 1 then widthToOrderedData.remove(maxK) else maxList.remove(maxList.size - 1)
          val num = data(maxI)
          val (left, filled) = (nbss - num, List.fill(num)(maxI.toString))
          val stop = widthToOrderedData.isEmpty || left < widthToOrderedData.firstKey
          if stop then (left, res ++ filled) else inner(left, res ++ filled)
        }
        val (left, filled) = inner(nbs, Nil)
        sb.appendAll(filled)
        (0 until left).foreach(_ => sb.append("."))
      })
      bi += 1
    }
    val sum = sb.indices.map(i => (if sb(i) == "." then 0 else sb(i).toInt).toLong * i).sum
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
