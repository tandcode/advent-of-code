package d09

import com.tandcode.adventofcode.y2023.d09.MirageMaintenance
import com.tandcode.adventofcode.y2023.d09.MirageMaintenance.{part1, part2}
import com.tandcode.adventofcode.y2023.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MirageMaintenanceTest extends AnyFlatSpec with Matchers {
  val test1: String = strFromResource("y2023/test09.txt")
  val input: String = strFromResource("y2023/input09.txt")

  "part1" should "pass with test data 1" in {
    part1(test1) should be(114)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(1806615041)
  }

  "part2" should "pass with test data" in {
    part2(test1) should be(2)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(1211)
  }
}
