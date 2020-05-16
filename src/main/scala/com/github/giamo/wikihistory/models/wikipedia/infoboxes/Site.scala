package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.wikipedia.{Coordinates, WikiPage}

final case class Site(
  pageId: Long,
  pageTitle: String,
  name: String,
  coordinates: Option[Coordinates] = None
) {
  val latitude: Option[Double] = coordinates.map(_.latitude)
  val longitude: Option[Double] = coordinates.map(_.longitude)
}

object Site extends Infobox[Site] {
  override val infoboxName = "(?:settlement|ancient site|greek dimos)"
  private val nameRegex = infoboxLinkRegex("name")
  private val coordinatesRegex = infoboxCoordinatesRegex("coordinates")

  override def fromInfobox(page: WikiPage): Option[Site] = {
    val text = page.text
    val cleanText = cleanInfoboxText(text)

    extractFromRegex(cleanText, nameRegex).map { name =>
      val coordinates = extractFromRegex(cleanText, coordinatesRegex)
      val anyCoordinates = coordinates
        .flatMap(Coordinates.fromTemplate)
        .fold(Coordinates.fromTemplate(text))(Some(_))

      Site(
        pageId = page.id,
        pageTitle = page.title,
        name = extractFromFormattedString(name),
        coordinates = anyCoordinates
      )
    }
  }
}