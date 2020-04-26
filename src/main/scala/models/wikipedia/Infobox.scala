package models.wikipedia

import scala.util.matching.Regex

trait Infobox[A] {
  def infoboxName: String

  def fromInfobox(text: String, fromPage: Long): Option[A]

  def cleanInfoboxText(text: String) = text.replace("\n", "$$")

  def infoboxFieldRegex(field: String): Regex =
    ("\\{\\{Infobox " + infoboxName + ".*?" + field + "[\\s]*=[\\s]*([^<${|]+)").r

  def infoboxNameRegex(field: String): Regex =
    ("\\{\\{Infobox " + infoboxName + ".*?" + field + "[\\s]*=[\\s]*([^<$]+)").r

  def infoboxCoordinatesRegex(field: String): Regex =
    ("\\{\\{Infobox " + infoboxName + ".*?" + field + "[\\s]*=[\\s]*([^$]+)").r

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

  private val formattedStringRegex = "\\{\\{([^\\}]+)[\\}]+".r
  def extractFromFormattedString(text: String): String = text match {
    case formattedStringRegex(value) => value.trim.split("\\|").last.trim
    case _ => text
  }
}
