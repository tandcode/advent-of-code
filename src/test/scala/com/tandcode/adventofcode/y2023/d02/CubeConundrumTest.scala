package com.tandcode.adventofcode.y2023.d02

import com.tandcode.adventofcode.y2023.d02.CubeConundrum
import com.tandcode.adventofcode.y2023.d02.CubeConundrum.{part1, part2}
import com.tandcode.adventofcode.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CubeConundrumTest extends AnyFlatSpec with Matchers {

  val test: String = strFromResource("y2023/test02.txt")
  val input: String = strFromResource("y2023/input02.txt")

  "part1" should "pass with test data" in {
    part1(test) should be(8)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(2085)
  }

  "part2" should "pass with test data" in {
    part2(test) should be(2286)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(79315)
  }
}