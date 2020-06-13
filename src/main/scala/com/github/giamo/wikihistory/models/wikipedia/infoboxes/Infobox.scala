package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.wikipedia.WikiPage

import scala.util.matching.Regex

trait Infobox[A] {
  def infoboxName: String

  def fromInfobox(page: WikiPage): Option[A]

  val infoboxTypeRegex: Regex = "(?s)(?i)\\{\\{Infobox\\s*([\\p{IsAlphabetic}\\s]+)\\s*.*".r

  def infoboxFieldRegex(field: String): Regex = (genericInfoboxField(field) + "\\s*=\\s*(.+?)\\s*(?:\\|[a-z0-9_\\s]+=.*|\n\\s*\\|\\s*\n|\\|\\||\\}\\}\\s*\n\\s*[^\\|*])").r

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
  private val listRegex3: Regex = "(?s)(?i)\\s*\\{\\{\\s*(?:unbulleted list\\s*\\|)(.+)\\}\\}\\s*".r
  private val listRegex4: Regex = "(?s)\\s*(.*\\*.*)\\s*".r

  def extractList(text: String): List[String] = {
    def extractListElems(elements: String, separator: String) = elements
      .split(s"\n?\\s*$separator\\s*")
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)

    text match {
      case listRegex1(elements) => extractListElems(elements, separator = "\\*")
      case listRegex2(elements) => extractListElems(elements, separator = "\\*")
      case listRegex3(elements) => extractListElems(elements, separator = "\\s+\\|")
      case listRegex4(elements) => extractListElems(elements, separator = "\\*")
      case _ => List(text)
    }
  }
}
