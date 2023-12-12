package com.tandcode.adventofcode.y2023.d05

import scala.annotation.tailrec

object SeedFertilizer {

  def part1(input: String): Long = closestSeed(input)

  def part2(input: String): Long = closestSeed2(input)

  val mapperRex = "(\\w+)-to-(\\w+) map:".r
  val theSeed = "seed"

  case class Range(start: Long, length: Long) extends Ordered[Range]{

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
          val nextValues = mapping
            .flatMap(line => {
              val Seq(dest, source, length) = line

              val value1 = valueRange
                .flatMap(value => {
                  val maybeTuple = value.intersect(Range(source, length))
                  maybeTuple
                    .map { case Range(start, len) => Range(start + dest - source, len) }
                }
                )
              value1
            })
            .distinct

          val notMappedValues = nextValues
            .foldLeft(valueRange) { case(out, value) => out.flatMap(_.subtract(value)) }

          val total = (nextValues ++ notMappedValues)
          val totalMerged = merge(total.sorted)
           loop(nextFrom, totalMerged)
    }

    val value = seeds.flatMap(seed => loop(theSeed, Seq(seed))).map { _.start }
    value.min
  }

  def merge(ranges: Seq[Range], prevSize: Int = 0): Seq[Range] =
    if (ranges.size == prevSize || ranges.size == 1)
    then ranges
    else {
      ranges.foldLeft(List.empty[Range]) { case(stack, el) =>
        if stack.isEmpty then el :: stack else stack.head.merge(el).reverse.toList ::: stack.tail
      }
    }

  def intersect(range1: (Long, Long),
                range2: (Long, Long)): Option[(Long, Long)] = {
    var r1 = range1
    var r2 = range2

    if (r1._1 > r2._1) {
      r1 = range2
      r2 = range1
    }

    val (start1, length1) = r1
    val (start2, length2) = r2
    val end1 = start1 + length1
    val end2 = start2 + length2
    val overlapStart = start2
    val overlapEnd = end1.min(end2)
    Option.when(overlapEnd > overlapStart)(overlapStart -> (overlapEnd - overlapStart))
  }

  def subtract(range1: (Long, Long),
               range2: (Long, Long)): Seq[(Long, Long)] = {
    val maybeIntersection = intersect(range1, range2)

    maybeIntersection.toSeq.flatMap(inter => {
      val (start1, length1) = range1
      val (startInt, lengthInt) = range2
      val end1 = start1 + length1
      val endInt = startInt + lengthInt

      Seq(
        (start1, startInt - start1),
        (endInt, end1 - endInt),
      )
    }).filter { case (_, length) => length > 0 }
  }

  def merge(range1: (Long, Long),
            range2: (Long, Long)): Seq[(Long, Long)] = {
    val (start1, length1) = range1
    val (start2, length2) = range2
    val end1 = start1 + length1
    val end2 = start2 + length2

    val isFirstSooner = start1 <= start2
    val shouldMerge = if isFirstSooner then end1 >= start2 else end2 >= start1
    if (shouldMerge) {
      val start = start1.min(start2)
      val length = end1.max(end2) - start1
      Seq(start -> length)
    } else {
      Seq(
        range1,
        range2
      )
    }
  }

}
