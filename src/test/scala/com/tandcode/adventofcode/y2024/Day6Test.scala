package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.TestUtil.readTestInputToRes
import org.scalatest.Inspectors.forEvery
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Tables.Table

class Day6Test extends AnyFlatSpec with Matchers {

  val input: Seq[(String, String, String)] = readTestInputToRes(this)

  it should "pass for part1" in {
    forEvery(input) { case (str, exp1, exp2) =>
      Day6.part1(str) shouldBe exp1.toLong
    }
  }

  it should "pass for part2" in {
    forEvery(input) { case (str, exp1, exp2) =>
      Day6.part2(str) shouldBe exp2.toLong
    }
  }

}
