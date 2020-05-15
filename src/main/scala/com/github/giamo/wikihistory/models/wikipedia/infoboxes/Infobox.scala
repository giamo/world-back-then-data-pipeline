package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.wikipedia.WikiPage

import scala.util.matching.Regex

trait Infobox[A] {
  def infoboxName: String

  def fromInfobox(page: WikiPage): Option[A]

  def cleanInfoboxText(text: String): String = text.replace("\n", "$$")

  def infoboxFieldRegex(field: String): Regex = (genericInfoboxField(field) + "[\\s]*=[\\s]*([^$]+)").r

  def infoboxLinkRegex(field: String): Regex = (genericInfoboxField(field) + "[\\s]*=[\\s]*([^<$]+)").r

  def infoboxCoordinatesRegex(field: String): Regex = (genericInfoboxField(field) + "[\\s]*=[\\s]*([^$]+)").r

  private val formattedStringRegex = "\\{\\{([^\\}]+)[\\}]+".r
  def extractFromFormattedString(text: String): String = text match {
    case formattedStringRegex(value) => value.trim.split("\\|").last.trim
    case _ => text
  }

  private def genericInfoboxField(field: String) = "(?i)\\{\\{Infobox " + infoboxName + ".*?" + field
}

object Infobox {
  def extractFromRegex(text: String, regex: Regex): Option[String] =
    regex
      .findAllIn(text)
      .matchData
      .toList
      .headOption
      .flatMap {
        _.subgroups match {
          case List(v) => Some(v.trim)
          case _       => None
        }
      }
}