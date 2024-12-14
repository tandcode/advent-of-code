package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.{Pos, ReachString}

import scala.annotation.tailrec

object Day14 {

  def part1(input: String): Long = {
    val (robots, grX, grY) = getRobotsWithGrid(input)
    val newRobots = robots.map(r => r.positionAfter(100, grX, grY))
    val xMid = grX / 2
    val yMid = grY / 2
    newRobots
      .filter(r => r.x != xMid && r.y != yMid)
      .groupBy(r => (r.x < xMid, r.y < yMid)).values
      .map(_.length)
      .product
  }

  def part2(input: String): Long = {
    val (robots, grX, grY) = getRobotsWithGrid(input)
    @tailrec
    def loop(steps: Int) : Int = {
      val positionsAt = robots.map(r => r.positionAfter(steps, grX, grY)).toSet
      val hasTr = hasTree(positionsAt)
      // using 10000 to satisfy unit test for test data
      if hasTr || steps > 10000 then steps else loop(steps + 1)
    }
    loop(1)
  }

  def getRobotsWithGrid(input: String): (Array[Robot], Int, Int) = {
    val number = "-?\\d+".r
    val lines = input.slines
    val robots = lines.init.map(line => {
      val Seq(pX, pY, vX, vY) = number.findAllIn(line).toSeq.map(_.toInt)
      Robot(pX, pY, vX, vY)
    })
    val Array(grX, grY) = lines.last.split(" ").map(_.toInt)
    (robots, grX, grY)
  }

  case class Robot(pX: Int, pY: Int, vX: Int, vY: Int) {
    def positionAfter(soconds: Int, gridX: Int, gridY: Int): Pos = {
      val newX = (vX * soconds % gridX + pX) % gridX
      val newY = (vY * soconds % gridY + pY) % gridY
      Pos(if newY < 0 then gridY + newY else newY, if newX < 0 then gridX + newX else newX)
    }
  }

  def hasTree(positions: Set[Pos]): Boolean = {
    val maxX = positions.groupBy(_.x).maxBy(_._2.size)._2.size
    val maxY = positions.groupBy(_.y).maxBy(_._2.size)._2.size
    maxY + maxX > 50
  }

}