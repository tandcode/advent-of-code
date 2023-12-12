package d04

import com.tandcode.adventofcode.y2023.d04.Scratchcards
import com.tandcode.adventofcode.y2023.d04.Scratchcards.{part1, part2}
import com.tandcode.adventofcode.y2023.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScratchcardsTest extends AnyFlatSpec with Matchers {

  val test: String = strFromResource("y2023/test04.txt")
  val input: String = strFromResource("y2023/input04.txt")

  "part1" should "pass with test data" in {
    part1(test) should be(13)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(15268)
  }

  "part2" should "pass with test data" in {
    part2(test) should be(30)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(6283755)
  }
}