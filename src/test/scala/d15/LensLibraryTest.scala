package d15

import com.tandcode.adventofcode.y2023.d15.LensLibrary
import com.tandcode.adventofcode.y2023.d15.LensLibrary.{part1, part2}
import com.tandcode.adventofcode.y2023.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LensLibraryTest extends AnyFlatSpec with Matchers {
  val test: String = strFromResource("y2023/test15.txt")
  val input: String = strFromResource("y2023/input15.txt")

  "part1" should "pass with test data" in {
    part1(test) should be(1320)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(512283)
  }

  "part2" should "pass with test data" in {
    part2(test) should be(145)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(215827)
  }
  
}
