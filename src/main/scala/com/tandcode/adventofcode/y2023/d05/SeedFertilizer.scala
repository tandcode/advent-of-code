package com.tandcode.adventofcode.y2023.d05

import scala.annotation.tailrec

object SeedFertilizer {

  def part1(input: String): Long = closestSeed(input)

  def part2(input: String): Long = closestSeed2(input)

  val mapperRex = "(\\w+)-to-(\\w+) map:".r
  val theSeed = "seed"

  case class Range(start: Long, length: Long) extends Ordered[Range]{


    override def toString: String = s"Range($start, ${end - 1})"

    // end excluded
    lazy val end = start + length

    override def compare(that: Range): Int = {
      val startDiff = start.compare(that.start)
      if startDiff == 0 then end.compare(that.end) else startDiff
    }

    def intersect(that: Range): Option[Range] = {
      var r1 = this
      var r2 = that

      if (r1.start > r2.start) {
        r1 = that
        r2 = this
      }

      val overlapEnd = r1.end.min(r2.end)
      Option.when(overlapEnd > r2.start)(Range(r2.start, overlapEnd - r2.start))
    }

    def subtract(that: Range): Seq[Range] = {
      intersect(that)
        .map(inter => Seq(
          Range(this.start, inter.start - this.start),
          Range(inter.end, this.end - inter.end),
        ).filter(_.length > 0)
      ).getOrElse(Seq(this))
    }

    def merge(that: Range): Seq[Range] = {
      val isThisSooner = this.start <= that.start
      val shouldMerge = if isThisSooner then this.end >= that.start else that.end >= this.start
      if (shouldMerge) {
        val start = this.start.min(that.start)
        val length = this.end.max(that.end) - this.start
        Seq(Range(start, length))
      } else {
        Seq(this, that)
      }
    }
  }

  def lineToLongs(line: String): Seq[Long] = line.trim.split("\\s+").map(_.toLong).toSeq

  def closestSeed(input: String): Long = {
    val inputBlocks = input.split("(\\r\\n){2,}")
    val seeds = lineToLongs(inputBlocks.head.substring(inputBlocks.head.indexOf(":") + 1))

    val mappings = inputBlocks.tail
      .map(block => {
        val lines = block.linesIterator.toSeq
        val mapperRex(from, to) = lines.head: @unchecked

        (from, to) -> lines.tail.map(lineToLongs)
      })
      .toMap

    val from2To = mappings.keys
      .map { case (from, to) => from -> to }
      .toMap

    val theSeed = "seed"

    @tailrec
    def loop(from: String, value: Long): Long = {
      val nextFrom = from2To.getOrElse(from, from)
      mappings.get(from -> nextFrom) match
        case None => value
        case Some(mapping) =>
          val nextValue = mapping
            .flatMap(line => {
              val Seq(dest, source, length) = line
              val inRange = value >= source && value < source + length
              Option.when(inRange)(value - source + dest)
            }).headOption.getOrElse(value)


          loop(nextFrom, nextValue)
    }

    seeds.map(seed => loop(theSeed, seed)).min
  }

  def closestSeed2(input: String): Long = {
    val inputBlocks = input.split("(\\r\\n){2,}")
    val seeds = lineToLongs(inputBlocks.head.substring(inputBlocks.head.indexOf(":") + 1))
      .sliding(2, 2).map { case Seq(start, length) => Range(start, length) }.toSeq

    val mappings = inputBlocks.tail
      .map(block => {
        val lines = block.linesIterator.toSeq
        val mapperRex(from, to) = lines.head: @unchecked

        (from, to) -> lines.tail.map(lineToLongs)
      })
      .toMap

    val from2To = mappings.keys
      .map { case (from, to) => from -> to }
      .toMap


    @tailrec
    def loop(from: String, valueRange: Seq[Range]): Seq[Range] = {
      val nextFrom = from2To.getOrElse(from, from)
      mappings.get(from -> nextFrom) match
        case None => valueRange
        case Some(mapping) =>
          val total = valueRange.flatMap(value => {
            val (notMapped, mapped) = mapping
              .foldLeft(Seq(value) -> Seq.empty[Range]) { case(notMapToRes, line) =>
                val (notMapped, mapped) = notMapToRes
                val Seq(dest, source, length) = line
                val r = Range(source, length)

                val mappedRanges = value.intersect(Range(source, length)).toSeq
                  .map { case Range(start, len) => Range(start + dest - source, len) }

                notMapped.flatMap(_.subtract(r)) -> (mapped ++ mappedRanges)
              }

            notMapped ++ mapped
          })

          loop(nextFrom, merge(total.sorted))
    }

    seeds.flatMap(seed => loop(theSeed, Seq(seed))).map(_.start).min
  }

  def merge(ranges: Seq[Range], prevSize: Int = 0): Seq[Range] =
    if (ranges.size == prevSize || ranges.size == 1)
    then ranges
    else {
      ranges.foldLeft(List.empty[Range]) { case(stack, el) =>
        if stack.isEmpty then el :: stack else stack.head.merge(el).reverse.toList ::: stack.tail
      }
    }

}
