package com.tandcode.adventofcode.y2024

import com.tandcode.adventofcode.TestUtil.readTestInputToRes
import org.scalatest.Inspectors.forEvery
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Tables.Table

class Day3Test extends AnyFlatSpec with Matchers {

  val input: Seq[(String, String, String)] = readTestInputToRes(this)

  it should "pass for part1" in {
    forEvery(input) { case (str, exp1, exp2) =>
      Day3.part1(str) shouldBe exp1.toInt
    }
  }

  it should "pass for part2" in {
    forEvery(input) { case (str, exp1, exp2) =>
      Day3.part2(str) shouldBe exp2.toInt
    }
  }

  val input2: Seq[(String, Seq[String])] = Table(
    ("Input", "Expected"),
    ("don't()a_don't()b_do()c", Seq("don't()a_", "do()c")),
    ("b_don't()a_don't()b_do()c", Seq("b_don't()a_", "do()c")),
    ("b_don't()a_do()g_do()k_don't()b_do()c", Seq("b_", "do()k_", "do()c")),
  )

  it should "pass for onlyDo" in {
    forEvery(input2) { case (str, exp) =>
      Day3.onlyDoINCORRECT(str) shouldBe exp
    }
  }

}
