package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.Date
import com.github.giamo.wikihistory.models.wikipedia.{Coordinates, WikiPage}

final case class Country(
  conventionalName: String,
  synopsis: String = "",
  name: Option[String] = None,
  yearStart: Option[String] = None,
  yearEnd: Option[String] = None,
  capital: Option[String] = None,
  coordinates: Option[Coordinates] = None,
  fromPage: Long
) {
  val parsedYearStart: Option[Date] = yearStart.flatMap(Date.fromString(_).toOption)
  val parsedYearEnd: Option[Date] = yearEnd.flatMap(Date.fromString(_).toOption)

  val toDateRangeString: Option[String] = for {
    s <- yearStart
    e <- yearEnd
  } yield s"$s - $e"

  def isIncludedInDateRange(from: Int, to: Int): Boolean = (
    for {
      s <- parsedYearStart
      e <- parsedYearEnd
    } yield s.toYear <= to && e.toYear >= from
    ).getOrElse(false)
}

object Country extends Infobox[Country] {
  override val infoboxName = "(?:country|former country|former subdivision)"
  private val nameRegex = infoboxLinkRegex("conventional_long_name")
  private val commonNameRegex = infoboxLinkRegex("common_name")
  private val yearStartRegex = infoboxFieldRegex("year_start")
  private val yearEndRegex = infoboxFieldRegex("year_end")
  private val coordinatesRegex = infoboxCoordinatesRegex("coordinates")

  // TODO: sometimes there's a list of capitals
  private val capitalRegex =
    ("(?i)\\{\\{Infobox " + infoboxName + ".*?capital[\\s]*=[\\s]*([^<${]+)").r

  override def fromInfobox(rawText: String, fromPage: Long): Option[Country] = {
    val cleanText = cleanInfoboxText(rawText)

    extractFromRegex(cleanText, nameRegex).map { conventionalName =>
      val commonName = extractFromRegex(cleanText, commonNameRegex)
      val yearStart = extractFromRegex(cleanText, yearStartRegex)
      val yearEnd = extractFromRegex(cleanText, yearEndRegex)
      val capital = extractFromRegex(cleanText, capitalRegex)
      val coordinates = extractFromRegex(cleanText, coordinatesRegex)

      val anyCoordinates = coordinates
        .flatMap(Coordinates.fromTemplate)
        .fold(Coordinates.fromTemplate(rawText))(Some(_))

      Country(
        conventionalName = extractFromFormattedString(conventionalName),
        synopsis = WikiPage.cleanSynopsis(rawText),
        name = commonName.map(extractFromFormattedString),
        yearStart = yearStart,
        yearEnd = yearEnd,
        capital = capital,
        coordinates = anyCoordinates,
        fromPage = fromPage
      )
    }
  }
}
