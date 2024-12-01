package com.tandcode.adventofcode.y2023.d11

import com.tandcode.adventofcode.y2023.d11.CosmicExpansion
import com.tandcode.adventofcode.y2023.d11.CosmicExpansion.{part1, part2}
import com.tandcode.adventofcode.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CosmicExpansionTest extends AnyFlatSpec with Matchers {
  val test: String = strFromResource("y2023/test11.txt")
  val input: String = strFromResource("y2023/input11.txt")

  "part1" should "pass with test data" in {
    part1(test) should be(374)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(9769724)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(603020563700L)
  }
}
