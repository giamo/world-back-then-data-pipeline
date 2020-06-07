package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.wikipedia.WikiPage

import scala.util.matching.Regex

trait Infobox[A] {
  def infoboxName: String

  def fromInfobox(page: WikiPage): Option[A]

  val infoboxTypeRegex: Regex = "(?s)(?i)\\{\\{Infobox\\s*([\\p{IsAlphabetic}\\s]+)\\s*.*".r

  def infoboxFieldRegex(field: String): Regex = (genericInfoboxField(field) + "\\s*=\\s*(.+?)\\s*\n\\s*(?:\\}\\}|\\|)").r

  def extractFromRegex(text: String, regex: Regex): Option[String] =
    regex
      .findAllIn(text)
      .matchData
      .toList
      .headOption
      .flatMap {
        _.subgroups match {
          case List(v) if !v.startsWith("|") => Some(v.trim)
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
  private val listRegex1: Regex = "(?s)(?i)\\s*\\{\\{\\s*(?:plainlist|flatlist|hlist)\\s*\\|\\s*(.+)\\}\\}\\s*".r
  private val listRegex2: Regex = "(?s)(?i)\\s*\\{\\{\\s*(?:plainlist|flatlist|hlist)\\s*\\}\\}(.+)\\{\\{\\s*(?:endplainlist|endflatlist|endhlist)\\s*\\}\\}\\s*".r
  private val listRegex3: Regex = "(?s)\\s*(.*\\*.*)\\s*".r

  def extractList(text: String): List[String] = {
    def extractListElems(elements: String) = elements
      .split("\n?\\s*\\*\\s*")
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)

    text match {
      case listRegex1(elements) => extractListElems(elements)
      case listRegex2(elements) => extractListElems(elements)
      case listRegex3(elements) => extractListElems(elements)
      case _ => List(text)
    }
  }
}
