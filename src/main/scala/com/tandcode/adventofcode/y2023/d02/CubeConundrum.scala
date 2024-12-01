package com.tandcode.adventofcode.y2023.d02

import com.tandcode.adventofcode.io.Util.strToLines

import scala.collection.mutable.HashMap as MHashMap

object CubeConundrum {

  def part1(input: String): Int = {
    val games = gameIdToGameSets(strToLines(input))
    countValidGames(games, totalCubes)
  }

  def part2(input: String): Int = {
    val games = gameIdToGameSets(strToLines(input))
    countMinPowers(games)
  }

  enum Color:
    case Red, Green, Blue

  val totalCubes: Map[Color, Int] = Map(
    Color.Red   -> 12,
    Color.Green -> 13,
    Color.Blue  -> 14
  )
  val gameRex = "\\s*Game (\\d+):\\s*(.*)".r

  def gameIdToGameSets(games: Seq[String]): Map[Int, Seq[Map[Color, Int]]] = {
    val gameIdToGameSets = for {
      game <- games
      gameRex(gameId, data) = game: @unchecked
      gameSets = data.split("\\s*;\\s*")
        .map(_.split("\\s*,\\s*")
          .map(_.split("\\s+"))
          .map { case Array(num, strColor) => Color.valueOf(strColor.capitalize) -> num.toInt }
          .toMap
        ).toSeq
    } yield gameId.toInt -> gameSets

    gameIdToGameSets.toMap
  }
  def countValidGames(gameIdToGameSets: Map[Int, Seq[Map[Color, Int]]], totalCubes: Map[Color, Int]): Int = {
    gameIdToGameSets
      .filter { case (gameId, gameSets) =>
        gameSets.flatten.forall { case (color, num) => totalCubes(color) >= num }
      }.keys.sum
  }

  def countMinPowers(gameIdToGameSets: Map[Int, Seq[Map[Color, Int]]]): Int = {
    gameIdToGameSets
      .map { case (gameId, gameSets) =>
        gameSets.flatten.foldLeft(new MHashMap[Color, Int]) { case (map, colorToNum) =>
          val (color, num) = colorToNum
          if map.getOrElseUpdate(color, num) < num then map.put(color, num)
          map
        }.values.product
      }.sum
  }
}
