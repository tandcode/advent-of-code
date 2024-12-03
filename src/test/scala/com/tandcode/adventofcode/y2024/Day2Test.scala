package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.TestUtil.readTestInputToRes
import org.scalatest.Inspectors.forEvery
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Test extends AnyFlatSpec with Matchers {

  val input: Seq[(String, String, String)] = readTestInputToRes(this)

  it should "pass for part1" in {
    forEvery(input) { case (str, exp1, exp2) =>
      Day2.part1(str) shouldBe exp1.toInt
    }
  }

  it should "pass for part2" in {
    forEvery(input) { case (str, exp1, exp2) =>
      Day2.part2(str) shouldBe exp2.toInt
    }
  }

}
