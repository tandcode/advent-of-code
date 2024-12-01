
package com.tandcode.adventofcode.y2023.d08

import com.tandcode.adventofcode.y2023.d08.HauntedWasteland
import com.tandcode.adventofcode.y2023.d08.HauntedWasteland.{part1, part2}
import com.tandcode.adventofcode.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HauntedWastelandTest extends AnyFlatSpec with Matchers {

  val test1: String = strFromResource("y2023/test081.txt")
  val test2: String = strFromResource("y2023/test082.txt")
  val test3: String = strFromResource("y2023/test083.txt")
  val input: String = strFromResource("y2023/input08.txt")

  "part1" should "pass with test data 1" in {
    part1(test1) should be(2)
  }

  "part1" should "pass with test data 2" in {
    part1(test2) should be(6)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(21389)
  }

  "part2" should "pass with test data" in {
    part2(test3) should be(6L)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(21083806112641L)
  }
}