package d13

import com.tandcode.adventofcode.y2023.d13.PointOfIncidence
import com.tandcode.adventofcode.y2023.d13.PointOfIncidence.{part1, part2}
import com.tandcode.adventofcode.y2023.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PointOfIncidenceTest extends AnyFlatSpec with Matchers {
  val test: String = strFromResource("y2023/test13.txt")
  val input: String = strFromResource("y2023/input13.txt")

  "part1" should "pass with test data" in {
    part1(test) should be(405)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(30487)
  }

  "part2" should "pass with test data" in {
    part2(test) should be(400)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(31954)
  }
  
}
