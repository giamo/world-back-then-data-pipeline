package models

import scala.util.matching.Regex

case class Country(
  conventionalName: String,
  name: Option[String],
  yearStart: Option[String],
  yearEnd: Option[String],
  capital: Option[String]
) {
  val parsedYearStart: Option[Int] = yearStart.flatMap(Date.fromString(_).toOption.map(_.toYear))
  val parsedYearEnd: Option[Int] = yearEnd.flatMap(Date.fromString(_).toOption.map(_.toYear))
}

object Country {
  private val nameRegex = formerCountryRegex("conventional_long_name")
  private val commonNameRegex = formerCountryRegex("common_name")
  private val yearStartRegex = formerCountryRegex("year_start")
  private val yearEndRegex = formerCountryRegex("year_end")

  // TODO: sometimes there's a list of capitals
  private val capitalRegex =
    "\\{\\{Infobox former country.*capital[\\s]*=[\\s\\[]*([^<${|\\]]+)".r

  def fromInfobox(infoboxText: String): Option[Country] = {
    val cleanText = infoboxText.replace("\n", "$$")

    extractFromRegex(cleanText, nameRegex).map { conventionalName =>
      val commonName = extractFromRegex(cleanText, commonNameRegex)
      val yearStart = extractFromRegex(cleanText, yearStartRegex)
      val yearEnd = extractFromRegex(cleanText, yearEndRegex)
      val capital = extractFromRegex(cleanText, capitalRegex)

      Country(
        conventionalName,
        commonName,
        yearStart,
        yearEnd,
        capital
      )
    }

  }

  private def formerCountryRegex(field: String): Regex =
    ("\\{\\{Infobox former country.*" + field + "[\\s]*=[\\s]*([^<${|]+)").r

  private def extractFromRegex(text: String, regex: Regex): Option[String] =
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
