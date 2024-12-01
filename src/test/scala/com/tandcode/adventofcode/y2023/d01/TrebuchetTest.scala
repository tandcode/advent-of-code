package com.tandcode.adventofcode.y2023.d01

import com.tandcode.adventofcode.y2023.d01.Trebuchet
import com.tandcode.adventofcode.y2023.d01.Trebuchet.{part1, part2}
import com.tandcode.adventofcode.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TrebuchetTest extends AnyFlatSpec with Matchers {

  val input: String = strFromResource("y2023/input01.txt")

  "part1" should "pass with test data" in {
    part1(strFromResource("y2023/test011.txt")) should be(142)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(54561)
  }

  "part2" should "pass with test data" in {
    part2(strFromResource("y2023/test012.txt")) should be(281)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(54076)
  }
}