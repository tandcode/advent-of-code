package com.tandcode.adventofcode.y2023.d10

import com.tandcode.adventofcode.y2023.d10.PipeMaze
import com.tandcode.adventofcode.y2023.d10.PipeMaze.{part1, part2}
import com.tandcode.adventofcode.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PipeMazeTest extends AnyFlatSpec with Matchers {
  val test1: String = strFromResource("y2023/test101.txt")
  val test2: String = strFromResource("y2023/test102.txt")
  val test3: String = strFromResource("y2023/test103.txt")
  val test4: String = strFromResource("y2023/test104.txt")
  val input: String = strFromResource("y2023/input10.txt")

  "part1" should "pass with test data 1" in {
    part1(test1) should be(4)
  }

  "part1" should "pass with test data 2" in {
    part1(test2) should be(8)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(6903)
  }

  "part2" should "pass with test data 3" in {
    part2(test3) should be(4)
  }

  "part2" should "pass with test data 4" in {
    part2(test4) should be(8)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(265)
  }
}
