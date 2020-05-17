package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.wikipedia.WikiPage

import scala.util.matching.Regex

trait Infobox[A] {
  def infoboxName: String

  def fromInfobox(page: WikiPage): Option[A]

  def infoboxFieldRegex(field: String): Regex = (genericInfoboxField(field) + "\\s*=\\s*(.+?)\\s*\n\\s*(?:\\}\\}|\\|)").r

  def extractFromRegex(text: String, regex: Regex): Option[String] =
    regex
      .findAllIn(text)
      .matchData
      .toList
      .headOption
      .flatMap {
        _.subgroups match {
          case List(v) => Some(v.trim)
          case _ => None
        }
      }

  private val formattedStringRegex = "\\{\\{([^\\}]+)[\\}]+".r

  def extractFromFormattedString(text: String): String = text match {
    case formattedStringRegex(value) => value.trim.split("\\|").last.trim
    case _ => text
  }

  private def genericInfoboxField(field: String) = "(?s)(?i)\\{\\{Infobox " + infoboxName + ".*?" + field
}

object Infobox {
  private val listRegex: Regex = "(?s)(?i)\\s*\\{\\{\\s*plainlist\\s*\\|\\s*(.+)\\}\\}\\s*".r

  def extractList(text: String): List[String] = text match {
    case listRegex(elements) => elements
      .split("\n?\\s*\\*\\s*")
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)
    case _ => List(text)
  }
}
