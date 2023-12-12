package d03

import com.tandcode.adventofcode.y2023.d03.GearRatios
import com.tandcode.adventofcode.y2023.d03.GearRatios.{part1, part2}
import com.tandcode.adventofcode.y2023.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GearRatiosTest extends AnyFlatSpec with Matchers {

  val test: String = strFromResource("y2023/test03.txt")
  val input: String = strFromResource("y2023/input03.txt")

  "part1" should "pass with test data" in {
    part1(test) should be(4361)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(538046)
  }

  "part2" should "pass with test data" in {
    part2(test) should be(467835)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(81709807)
  }
}