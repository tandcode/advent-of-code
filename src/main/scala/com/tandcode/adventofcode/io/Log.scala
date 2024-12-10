package com.tandcode.adventofcode.io

object Log {
  
  def withTimeLog[T](run: => T): T = {
    val start = System.nanoTime()
    val res = run
    val elapsed = System.nanoTime() - start
    println(s"Execution took: ${elapsed.doubleValue / 1_000_000_000} sec")
    res
  }
  
}
