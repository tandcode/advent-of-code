package com.tandcode.adventofcode.y2023.d12

import com.tandcode.adventofcode.y2023.d12.HotSprings
import com.tandcode.adventofcode.y2023.d12.HotSprings.{part1, part2}
import com.tandcode.adventofcode.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HotSpringsTest extends AnyFlatSpec with Matchers {
  val test: String = strFromResource("y2023/test12.txt")
  val testt: String = strFromResource("y2023/test12t.txt")
  val inputt: String = strFromResource("y2023/input12t.txt")
  val input: String = strFromResource("y2023/input12.txt")

  "part1" should "pass with test data" in {
    part1(test) should be(21)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(7110)
  }

  "part2" should "pass with test data" in {
    part2(test) should be(525152)
  }

//  "part2" should "pass with test test data" in {
//    part2(testt) should be(525152)
//  }
//
//  "part2" should "pass with input data" in {
//    part2(input) should be(525152)
//  }
  
}
