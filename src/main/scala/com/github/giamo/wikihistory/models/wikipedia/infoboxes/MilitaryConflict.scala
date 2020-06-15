package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.wikipedia.{Coordinates, PageTitle, WikiPage}

final case class MilitaryConflict(
  pageId: Long,
  pageTitle: String,
  infoboxType: String,
  conflict: String,
  synopsis: String = "",
  partOf: Option[String] = None,
  date: Option[String] = None,
  place: Option[String] = None,
  coordinates: Option[Coordinates] = None
)

object MilitaryConflict extends Infobox[MilitaryConflict] {
  override val infoboxName = "military conflict"
  private val conflictRegex = infoboxFieldRegex("conflict")
  private val partOfRegex = infoboxFieldRegex("partof")
  private val dateRegex = infoboxFieldRegex("date")
  private val placeRegex = infoboxFieldRegex("place")
  private val coordinatesRegex = infoboxFieldRegex("coordinates")

  override def fromInfobox(page: WikiPage): Option[MilitaryConflict] = {
    val rawText = page.text

    for {
      infoboxType <- extractFromRegex(rawText, infoboxTypeRegex)
      conflict <- extractFromRegex(rawText, conflictRegex)
    } yield {
      val partOf = extractFromRegex(rawText, partOfRegex)
      val date = extractFromRegex(rawText, dateRegex)
      val place = extractFromRegex(rawText, placeRegex)
      val coordinates = extractFromRegex(rawText, coordinatesRegex)

      val anyCoordinates = coordinates
        .flatMap(Coordinates.fromTemplate)
        .fold(Coordinates.fromTemplate(rawText))(Some(_))

      MilitaryConflict(
        pageId = page.id,
        pageTitle = page.title,
        infoboxType = infoboxType,
        conflict = extractFromFormattedString(conflict),
        partOf = partOf,
        date = date,
        place = place,
        coordinates = anyCoordinates,
        synopsis = WikiPage.getHtmlSynopsis(rawText)
      )
    }
  }
}

