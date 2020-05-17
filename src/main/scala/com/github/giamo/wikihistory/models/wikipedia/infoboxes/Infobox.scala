package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.wikipedia.WikiPage

import scala.util.matching.Regex

trait Infobox[A] {
  def infoboxName: String

  def fromInfobox(page: WikiPage): Option[A]

  def infoboxFieldRegex(field: String): Regex = (genericInfoboxField(field) + "\\s*=\\s*(.+?)\\s*\n\\s*(?:\\}\\}|\\|)").r

  def infoboxListRegex(field: String): Regex =
    (genericInfoboxField(field) + "\\s*=\\s*\\{\\{plainlist\\s*\\|\\s*(.+?)\\}\\}\\s*\\$\\$\\s*(?:\\}\\}$|\\|)").r

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

  def extractListFromRegex(text: String, field: String): List[String] =
    infoboxListRegex(field)
      .findAllIn(text)
      .matchData.toList
      .headOption
      .map { r =>
        r.group(1)
          .split("\\|?\\s*\\*")
          .map(_.replaceAll("\\$\\$", "").trim)
          .toList
          .filter(_.nonEmpty)
      }
      .getOrElse(
        extractFromRegex(text, infoboxFieldRegex(field))
          .map {
            case s if s == null || s.trim.isEmpty => List.empty
            case s => List(s.trim)
          }
          .getOrElse(List.empty[String])
      )

  private val formattedStringRegex = "\\{\\{([^\\}]+)[\\}]+".r

  def extractFromFormattedString(text: String): String = text match {
    case formattedStringRegex(value) => value.trim.split("\\|").last.trim
    case _ => text
  }

  private def genericInfoboxField(field: String) = "(?s)(?i)\\{\\{Infobox " + infoboxName + ".*?" + field
}
