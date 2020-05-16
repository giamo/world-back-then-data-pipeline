package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.Date
import com.github.giamo.wikihistory.models.wikipedia.{Capital, Coordinates, WikiPage}

final case class Country(
  pageId: Long,
  pageTitle: String,
  conventionalName: String,
  synopsis: String = "",
  name: Option[String] = None,
  yearStart: Option[String] = None,
  yearEnd: Option[String] = None,
  capital: Option[String] = None,
  coordinates: Option[Coordinates] = None,
  governmentType: Option[String] = None,
  imageCoat: Option[String] = None,
  commonLanguages: Option[String] = None,
  religion: Option[String] = None
) {
  val parsedYearStart: Option[Date] = yearStart.flatMap(Date.fromString(_).toOption)
  val parsedYearEnd: Option[Date] = yearEnd.flatMap(Date.fromString(_).toOption)
//  val parsedCapitals: List[Capital] = capitals.map(Capital.fromString)

  val toDateRangeString: Option[String] = for {
    s <- yearStart
    e <- yearEnd
  } yield s"$s - $e"

  def isIncludedInDateRange(from: Int, to: Int): Boolean = (
    for {
      s <- parsedYearStart
      e <- parsedYearEnd
    } yield s.toYear <= from && e.toYear >= to
    ).getOrElse(false)
}

object Country extends Infobox[Country] {
  override val infoboxName = "(?:country|former country|former subdivision)"
  private val nameRegex = infoboxFieldRegex("conventional_long_name")
  private val commonNameRegex = infoboxFieldRegex("common_name")
  private val yearStartRegex = infoboxFieldRegex("year_start")
  private val yearEndRegex = infoboxFieldRegex("year_end")
  private val coordinatesRegex = infoboxFieldRegex("coordinates")
  private val capitalRegex = infoboxFieldRegex("capital")
  private val governmentRegex = infoboxFieldRegex("government_type")
  private val imageCoatRegex = infoboxFieldRegex("image_coat")
  private val languagesRegex = infoboxFieldRegex("common_languages")
  private val religionRegex = infoboxFieldRegex("religion")

  override def fromInfobox(page: WikiPage): Option[Country] = {
    val rawText = page.text
    val cleanText = cleanInfoboxText(rawText)

    extractFromRegex(cleanText, nameRegex).map { conventionalName =>
      val commonName = extractFromRegex(cleanText, commonNameRegex)
      val yearStart = extractFromRegex(cleanText, yearStartRegex)
      val yearEnd = extractFromRegex(cleanText, yearEndRegex)
      val capitals = extractFromRegex(cleanText, capitalRegex)
      val coordinates = extractFromRegex(cleanText, coordinatesRegex)
      val government = extractFromRegex(cleanText, governmentRegex)
      val imageCoat = extractFromRegex(cleanText, imageCoatRegex)
      val languages = extractFromRegex(cleanText, languagesRegex)
      val religions = extractFromRegex(cleanText, religionRegex)

      val anyCoordinates = coordinates
        .flatMap(Coordinates.fromTemplate)
        .fold(Coordinates.fromTemplate(rawText))(Some(_))

      Country(
        pageId = page.id,
        pageTitle = page.title,
        conventionalName = extractFromFormattedString(conventionalName),
        synopsis = WikiPage.getHtmlSynopsis(rawText),
        name = commonName.map(extractFromFormattedString),
        yearStart = yearStart,
        yearEnd = yearEnd,
        capital = capitals,
        coordinates = anyCoordinates,
        governmentType = government,
        imageCoat = imageCoat,
        commonLanguages = languages,
        religion = religions
      )
    }
  }
}
