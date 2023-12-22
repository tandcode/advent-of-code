
package d05

import com.tandcode.adventofcode.y2023.d05.SeedFertilizer
import com.tandcode.adventofcode.y2023.d05.SeedFertilizer.{part1, part2}
import com.tandcode.adventofcode.y2023.io.Util.strFromResource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SeedFertilizerTest extends AnyFlatSpec with Matchers {

  val test: String = strFromResource("y2023/test05.txt")
  val input: String = strFromResource("y2023/input05.txt")
  val danyInput: String = strFromResource("y2023/test05v2.txt")

  "part1" should "pass with test data" in {
    part1(test) should be(35)
  }

  "part1" should "pass with input data" in {
    part1(input) should be(318728750L)
  }

  "part2" should "pass with test data" in {
    part2(test) should be(46)
  }

  "part2" should "pass with input data" in {
    part2(input) should be(37384986L)
  }

  "part2" should "pass with dany data" in {
    part2(danyInput) should be(2008785)
  }

}