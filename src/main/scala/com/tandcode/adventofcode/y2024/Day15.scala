package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.api.Direction.*
import com.tandcode.adventofcode.api.{Direction, Pos, ReachGridStr, ReachString}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Day15 {

  def part1(input: String): Long = calculate(input, identity, _ == 'O', 'O', push)

  def part2(input: String): Long = calculate(input, grow, c => c == '[' || c == ']', '[', push2)

  private def calculate(input: String,
                        gridModifier: Array[String] => Array[String],
                        isBox: Char => Boolean,
                        boxToCount: Char,
                        pushFn: (Array[String], Pos, Direction) => Boolean): Long = {
    val Array(rawGrid, moves) = input.parts()
    val cleanGrid = rawGrid.slines
    val directions = moves.split("").filterNot(_ == "\n").map(dir => Direction(dir.head))

    val grid = gridModifier(cleanGrid)
    val start = grid.find('@')
    grid.set(start, '.')

    directions.foldLeft((grid, start)) { case ((grid, pos), dir) =>
      val next = pos(dir)
      if !next.validForGrid(grid) then (grid, pos) else grid(next) match
        case c: '#' => (grid, pos)
        case '.' => (grid, next)
        case c if isBox(c) => if pushFn(grid, pos, dir) then (grid, next) else (grid, pos)
    }
    grid.positionsOf(boxToCount)
      .map(p => p.y * 100 + p.x)
      .sum
  }

  def push(grid: Array[String], pos: Pos, dir: Direction): Boolean = {
    val posN = pos(dir)
    @tailrec
    def takeNextWhile(cur: Pos): Pos = {
      val next = cur(dir)
      if !next.validForGrid(grid) || grid(next) != grid(posN) then next else takeNextWhile(next)
    }

    val next = takeNextWhile(posN)
    val wasPushed = next.validForGrid(grid) && grid(next) != '#'
    if wasPushed then grid.swap(posN, next)
    wasPushed
  }

  def grow(grid: Array[String]): Array[String] = {
    grid.map(line => {
      line.foldLeft((new ArrayBuffer[Char]())) { case(newLine, c) => c match
        case 'O' =>
          newLine += '['
          newLine += ']'
        case '@' =>
          newLine += '@'
          newLine += '.'
        case c =>
          newLine += c
          newLine += c
        newLine
      }.mkString
    })
  }

  def push2(grid: Array[String], pos: Pos, dir: Direction): Boolean = {
    def affectedPositions(cur: Pos): (Set[Pos], Boolean) = {
      val next = cur(dir)
      val nextValid = next.validForGrid(grid)
      if nextValid && (grid(next) == '[' || grid(next) == ']') then {
        val neighbour = if grid(next) == '[' then next.right else next.left
        val (nextAffected, nextBlocked) = affectedPositions(next)
        if (nextBlocked) return (Set.empty, true)
        val (neighbourAffected, neighbourBlocked) = affectedPositions(neighbour)
        if (neighbourBlocked) return (Set.empty, true)

        (nextAffected ++ neighbourAffected ++ Set(next, neighbour), nextBlocked || neighbourBlocked)
      } else {
        (Set.empty, !nextValid || grid(next) != '.')
      }
    }

    @tailrec
    def isPosBlocked(p: Pos, i: Int = 0): Int = {
      val next = p(dir)
      if !next.validForGrid(grid) || grid(next) != '.' then i else isPosBlocked(next, i + 1)
    }

    val next = pos(dir)
    if (next.y == pos.y) {
      return pushHorizontal(grid, next, dir)
    }

    val (affected, isBlocked) = affectedPositions(pos)

    if !isBlocked then {
      val pToV = affected.map(p => p -> grid(p)).toMap
      affected.foreach(p => grid.set(p(dir), pToV(p)))
      val newPositions = affected.map(p => p(dir))
      val leftClean = affected -- newPositions
      leftClean.foreach(p => grid.set(p, '.'))
    }
    !isBlocked
  }

  def pushHorizontal(grid: Array[String], pos: Pos, dir: Direction): Boolean = {
    val opos: Char => Char = c => if c == '[' then ']' else '['

    @tailrec
    def takeNextWhile(cur: Pos): Pos = {
      val next = cur(dir)
      if !next.validForGrid(grid) || (grid(next) != '[' && grid(next) != ']') then next else takeNextWhile(next)
    }

    val nextFn: Pos => Pos = _(dir)
    val next = takeNextWhile(pos)
    val wasPushed = next.validForGrid(grid) && grid(next) != '#'

    @tailrec
    def rotate(pos: Pos, end: Pos, dir: Direction): Unit = if pos == end then () else {
      val next = pos(dir)
      grid.swap(pos, next)
      rotate(next, end, dir)
    }

    if wasPushed then rotate(next, pos, dir.turnAround)

    wasPushed
  }

}