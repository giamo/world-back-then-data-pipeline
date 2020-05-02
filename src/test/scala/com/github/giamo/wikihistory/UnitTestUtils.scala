package com.github.giamo.wikihistory

import com.github.giamo.wikihistory.models.wikipedia.WikiPage

import scala.io.Source

object UnitTestUtils {
  def readFromFile(path: String): String = Source
    .fromResource(path)
    .getLines()
    .mkString("\n")

  def sampleWikiPage(id: Long, title: String, text: String) = WikiPage(
    id = id,
    title = title,
    text = text,
    isCategory = false,
    isFile = false,
    isTemplate = false
  )
}
