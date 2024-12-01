
package com.tandcode.adventofcode.y2023.d07

import com.tandcode.adventofcode.y2023.d07.CamelCards
import com.tandcode.adventofcode.y2023.d07.CamelCards.{part1, part2}
import com.tandcode.adventofcode.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CamelCardsTest extends AnyFlatSpec with Matchers {

  val test: String = strFromResource("y2023/test07.txt")
  val input: String = strFromResource("y2023/input07.txt")

  "part1" should "pass with test data" in {
    part1(test) should be(6440)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(250232501)
  }

  "part2" should "pass with test data" in {
    part2(test) should be(5905)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(249138943)
  }
}