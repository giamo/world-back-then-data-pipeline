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
  override val infoboxName = "(?:settlement|ancient site|greek dimos|swiss town|italian comune|german location|russian inhabited locality|military installation)"
  private val nameRegex = infoboxFieldRegex("name")
  private val coordinatesRegex = infoboxFieldRegex("coordinates")

  override def fromInfobox(page: WikiPage): Option[Site] = {
    val rawText = page.text

    extractFromRegex(rawText, nameRegex).map { name =>
      val coordinates = extractFromRegex(rawText, coordinatesRegex)
      val anyCoordinates = coordinates
        .flatMap(Coordinates.fromTemplate)
        .fold(Coordinates.fromTemplate(rawText))(Some(_))

      Site(
        pageId = page.id,
        pageTitle = page.title,
        name = extractFromFormattedString(name),
        coordinates = anyCoordinates
      )
    }
  }
}