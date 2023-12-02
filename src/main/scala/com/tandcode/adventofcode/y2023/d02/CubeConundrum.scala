package com.tandcode.adventofcode.y2023.d02

import com.tandcode.adventofcode.y2023.d02.CubeConundrum.Color

import scala.io.Source
import scala.util.Using
import scala.collection.mutable.HashMap as MHashMap



object CubeConundrum extends App {

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

  val testValues = Seq(
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  )
  val inputValues: Seq[String] = Using(Source.fromResource("y2023/d02/input.txt"))(_.getLines().toSeq).getOrElse(Nil)

  val testGames: Map[Int, Seq[Map[Color, Int]]] = gameIdToGameSets(testValues)
  val inputGames: Map[Int, Seq[Map[Color, Int]]] = gameIdToGameSets(inputValues)

  println(s"Count valid games test: ${countValidGames(testGames, totalCubes)}")
  println(s"Count valid games: ${countValidGames(inputGames, totalCubes)}")

  println(s"Power of cubes sum test: ${countMinPowers(testGames)}")
  println(s"Power of cubes sum: ${countMinPowers(inputGames)}")
}
