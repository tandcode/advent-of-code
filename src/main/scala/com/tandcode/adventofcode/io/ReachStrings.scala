package com.tandcode.adventofcode.io

implicit class ReachStrings(s: Array[String]) {
  
  def numbers: Array[Int] = s.map(_.toInt)
  
}
