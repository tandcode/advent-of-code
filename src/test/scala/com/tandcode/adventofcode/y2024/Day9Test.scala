package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.TestUtil.readTestInputToRes
import org.scalatest.Inspectors.forEvery
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Tables.Table

class Day9Test extends AnyFlatSpec with Matchers {

  val input: Seq[(String, String, String)] = readTestInputToRes(this)

  it should "pass for part1" in {
    forEvery(input) { case (str, exp1, exp2) =>
      Day9.part1Gold(str) shouldBe exp1
    }
  }

  it should "pass for part2" in {
    forEvery(input) { case (str, exp1, exp2) =>
      Day9.part2(str) shouldBe exp2
    }
  }

}
