package d05

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.tandcode.adventofcode.y2023.d05.SeedFertilizer.Range

class RangeTest extends AnyFlatSpec with Matchers {

  "Range.intersect" should "correctly intersect if this first" in {
    Range(5, 10) intersect Range(10, 15) should be(Some(Range(10, 5)))
    Range(5, 10) intersect Range(10, 5) should be(Some(Range(10, 5)))
  }

  "Range.intersect" should "correctly intersect if none first" in {
    Range(5, 10) intersect Range(5, 15) should be(Some(Range(5, 10)))
    Range(5, 15) intersect Range(5, 10) should be(Some(Range(5, 10)))
  }

  "Range.intersect" should "correctly intersect if that first" in {
    Range(10, 15) intersect Range(5, 10) should be(Some(Range(10, 5)))
    Range(10, 5) intersect Range(5, 10) should be(Some(Range(10, 5)))
  }

  "Range.intersect" should "correctly intersect if this in the middle" in {
    Range(10, 5) intersect Range(5, 15) should be(Some(Range(10, 5)))
    }

  "Range.intersect" should "correctly intersect if that in the middle" in {
    Range(5, 15) intersect Range(10, 5) should be(Some(Range(10, 5)))
  }


  "Range.subtract" should "correctly intersect if this first" in {
    Range(5, 10) subtract Range(10, 15) should be(Seq(Range(5, 5)))
    Range(5, 10) subtract Range(10, 5) should be(Seq(Range(5, 5)))
  }

  "Range.subtract" should "correctly intersect if none first" in {
    Range(5, 10) subtract Range(5, 15) should be(Nil)
    Range(5, 15) subtract Range(5, 10) should be(Seq(Range(15, 5)))
  }

  "Range.subtract" should "correctly intersect if that first" in {
    Range(10, 15) subtract Range(5, 10) should be(Seq(Range(15, 10)))
    Range(10, 5) subtract Range(5, 10) should be(Nil)
  }

  "Range.subtract" should "correctly intersect if this in the middle" in {
    Range(10, 5) subtract Range(5, 15) should be(Nil)
  }

  "Range.subtract" should "correctly intersect if that in the middle" in {
    Range(5, 15) subtract Range(10, 5) should be(Seq(Range(5, 5), Range(15, 5)))
  }
}
