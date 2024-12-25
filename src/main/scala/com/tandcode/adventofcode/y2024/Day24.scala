package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.ReachString

import scala.annotation.tailrec
import scala.collection.mutable


object Day24 {

  def part1(input: String): Long = {
    val (regToVal, sortedByDependency) = prepareInput(input)
    sortedByDependency.foreach((v1, op, v2, r) => {
      val val1 = regToVal(v1)
      val val2 = regToVal(v2)
      val oper: (Int, Int) => Int = op match
        case "AND" => _ & _
        case "OR" => _ | _
        case "XOR" => _ ^ _
      val res = oper(val1, val2)
      regToVal.put(r, res)
    })
    val binaryRes = regToVal
      .filter((k, v) => k.startsWith("z")).toSeq
      .sortBy((k, _) => k)
      .map(_._2)
      .reverse
      .mkString

    java.lang.Long.parseLong(binaryRes, 2)
  }

  private def countDependencies(opers: Array[(String, String, String, String)]): Map[String, Set[String]] = {
    val keyToRawDeps = opers.map((v1, _, v2, r) => r -> Set(v1, v2)).toMap
    val dependencies = new mutable.HashMap[String, Set[String]]()
    @tailrec
    def addDep(deps: Set[String]): Set[String] = {
      val newDeps = deps.flatMap(d => keyToRawDeps.getOrElse(d, Set.empty[String]) + d)
      if newDeps.size == deps.size then newDeps else addDep(newDeps)
    }

    keyToRawDeps.map((k, vv) => {
      k -> vv.flatMap(v => if dependencies.contains(v) then dependencies(v) else {
        val deps = addDep(Set(v))
        dependencies.put(v, deps)
        deps
      })
    })
  }

  def part2(input: String): String = {
    val (regToVal, sortedByDependency) = prepareInput(input)
    sortedByDependency.foreach((v1, op, v2, r) => {
      val val1 = regToVal(v1)
      val val2 = regToVal(v2)
      val oper: (Int, Int) => Int = op match
        case "AND" => _ & _
        case "OR" => _ | _
        case "XOR" => _ ^ _
      val res = oper(val1, val2)
      regToVal.put(r, res)
    })
    val xS = regToVal.filter((k, _) => k.startsWith("x"))
    val yS = regToVal.filter((k, _) => k.startsWith("y"))
    val zS = regToVal.filter((k, _) => k.startsWith("z"))
    val tuples = xS.zip(yS).zip(zS)
    val binaryRes = regToVal
      .filter((k, v) => k.startsWith("z")).toSeq
      .sortBy((k, _) => k)
      .map(_._2)
      .reverse
      .mkString

    java.lang.Long.parseLong(binaryRes, 2)
    ""
  }

  private def prepareInput(input: String): (mutable.Map[String, Int], Array[(String, String, String, String)]) = {
    val Array(initialState, operations) = input.parts()
    val regToVal = new mutable.TreeMap[String, Int]()
    initialState.slines.foreach(l => {
      val Array(k, v) = l.split(": ")
      regToVal.put(k, v.toInt)
    })
    val opers: Array[(String, String, String, String)] = operations.slines.map(op => {
      val Array(v1, oper, v2, _, reg) = op.split(" ")
      (v1, oper, v2, reg)
    })
    val depends = countDependencies(opers)
    val sortedByDependency = opers.sorted((v1, v2) => {
      val dep1 = depends(v1._4)
      val dep2 = depends(v2._4)
      if dep1(v2._4) then 1
      else if dep2(v1._4) then -1
      else dep1.size - dep2.size
    })
    (regToVal, sortedByDependency)
  }

  private def calcConnections(input: String): mutable.HashMap[String, mutable.Set[String]] = {
    input.slines.foldLeft(new mutable.HashMap[String, mutable.Set[String]]()) { case (map, line) =>
      val Array(c1, c2) = line.split("-")
      map.getOrElseUpdate(c1, new mutable.HashSet[String]()) += c2
      map.getOrElseUpdate(c2, new mutable.HashSet[String]()) += c1
      map
    }
  }

}
