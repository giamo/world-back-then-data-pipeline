package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.wikipedia.Coordinates

final case class Site(name: String, coordinates: Option[Coordinates] = None, fromPage: Long) {
  val latitude: Option[Double] = coordinates.map(_.latitude)
  val longitude: Option[Double] = coordinates.map(_.longitude)
}

object Site extends Infobox[Site] {
  override val infoboxName = "(?:settlement|ancient site|Greek Dimos)"
  private val nameRegex = infoboxLinkRegex("name")
  private val coordinatesRegex = infoboxCoordinatesRegex("coordinates")

  override def fromInfobox(text: String, fromPage: Long): Option[Site] = {
    val cleanText = cleanInfoboxText(text)

    extractFromRegex(cleanText, nameRegex).map { name =>
      val coordinates = extractFromRegex(cleanText, coordinatesRegex)
      Site(
        extractFromFormattedString(name),
        coordinates.flatMap(Coordinates.fromTemplate),
        fromPage
      )
    }
  }
}