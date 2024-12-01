package com.tandcode.adventofcode.y2023.d14

import com.tandcode.adventofcode.y2023.d14.ParabolicReflector
import com.tandcode.adventofcode.y2023.d14.ParabolicReflector.{part1, part2}
import com.tandcode.adventofcode.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParabolicReflectorTest extends AnyFlatSpec with Matchers {
  val test: String = strFromResource("y2023/test14.txt")
  val input: String = strFromResource("y2023/input14.txt")

  "part1" should "pass with test data" in {
    part1(test) should be(136)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(103614)
  }

  "part2" should "pass with test data" in {
    part2(test) should be(64)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(83790)
  }
  
}
