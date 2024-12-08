package com.tandcode.adventofcode.y2023.d17.d15

import com.tandcode.adventofcode.io.Util.strFromResource
import com.tandcode.adventofcode.y2023.d15.LensLibrary
import com.tandcode.adventofcode.y2023.d15.LensLibrary.{part1, part2}
import com.tandcode.adventofcode.y2023.d17.Day17
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17Test extends AnyFlatSpec with Matchers {
  val test: String = strFromResource("y2023/test17.txt")
  val test2: String = strFromResource("y2023/test172.txt")
  val input: String = strFromResource("y2023/test171.txt")

  "part1" should "pass with test data" in {
    Day17.part1(test) should be(102)
  }

  "part1" should "pass with input data" in {
    Day17.part1(input) should be(953)
  }

  "part2" should "pass with test data" in {
    Day17.part2(test) should be(94)
  }

  "part2" should "pass with input data" in {
    Day17.part2(input) should be(215827)
  }
  
}
