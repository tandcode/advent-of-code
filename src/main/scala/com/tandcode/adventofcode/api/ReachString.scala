package com.tandcode.adventofcode.api

implicit class ReachString(s: String) {
  
  def slines: Array[String] = parts(1)

  def parts(sep: Int = 2): Array[String] = s.split("\n" * sep)
  
}
