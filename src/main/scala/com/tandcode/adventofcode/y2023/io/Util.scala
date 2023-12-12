package com.tandcode.adventofcode.y2023.io

import scala.io.Source
import scala.util.Using

object Util {
  def strFromResource(fileName: String): String = Using(Source.fromResource(fileName))(_.mkString).getOrElse("")
  
  def linesFromResource(fileName: String): Seq[String] = Using(Source.fromResource(fileName))(_.getLines().toSeq)
    .getOrElse(Nil)
  
  def strToLines(string: String): Seq[String] = string.split("[\r\n]+").toSeq
}
