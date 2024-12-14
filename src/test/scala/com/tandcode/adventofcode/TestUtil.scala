package com.tandcode.adventofcode

import com.tandcode.adventofcode.io.Util.{listFilesByPrefix, strFromFile}
import org.scalatest.prop.TableFor3
import org.scalatest.prop.Tables.Table

object TestUtil {
  def readTestInputToRes(testClassObj: Any): TableFor3[String, String, String] = {
    val yearFromPackage = testClassObj.getClass.getPackage.getName.split("\\.").last
    val testFiles = listFilesByPrefix(s"$yearFromPackage/${testClassObj.getClass.getSimpleName.stripSuffix("$")}")
    val inpToRes = testFiles
      .sortBy(f => f.getName)
      .map(f => {
        val testInput = strFromFile(f)
        val sep1 = nLastIndexOf(testInput, '\n', 1)
        val sep2 = nLastIndexOf(testInput, '\n')

        (testInput.substring(0, sep1), testInput.substring(sep1 + 1, sep2), testInput.substring(sep2 + 1))
      })

    Table(
      ("Input", "Expected1", "Expected2"),
      inpToRes: _*
    )
  }

  def nLastIndexOf(str: String, c: Char, i: Int = 0): Int = {
    var curN = -1

    var j = str.length - 1
    while (j >= 0) {
      if (str.charAt(j) == c) {
        curN += 1
      }
      if (curN == i) {
        return j
      }
      j -= 1
    }
    -1
  }
}
