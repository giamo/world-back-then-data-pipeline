package com.github.giamo.wikihistory

import scala.io.Source

object UnitTestUtils {
  def readFromFile(path: String): String = Source
    .fromResource(path)
    .getLines()
    .mkString("\n")
}
