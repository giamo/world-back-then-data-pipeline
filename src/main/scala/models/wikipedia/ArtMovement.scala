package models.wikipedia

import models.{DateRange, ParseError}

import scala.util.matching.Regex

final case class ArtMovement(name: String, yearsactive: Option[String]) {
  val parsedYearsActive = yearsactive.flatMap(DateRange.fromString(_).toOption)
}

object ArtMovement {
  private val nameRegex = artMovementRegex("name")
  private val yearsRegex = artMovementRegex("yearsactive")

  def fromInfobox(infoboxText: String): Option[ArtMovement] = {
    val cleanText = infoboxText.replace("\n", "$$")

    extractFromRegex(cleanText, nameRegex).map { name =>
      val yearsActive = extractFromRegex(cleanText, yearsRegex)
      ArtMovement(name, yearsActive)
    }
  }

  private def artMovementRegex(field: String): Regex =
    ("\\{\\{Infobox art movement.*?" + field + "[\\s]*=[\\s]*([^<${|]+)").r

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
