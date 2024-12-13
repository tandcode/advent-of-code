package com.tandcode.adventofcode.api

implicit class ReachStrings(s: Array[String]) {
  
  def numbers: Array[Int] = s.map(_.toInt)
  
}
