package models.wikipedia

final case class Site(name: String, coordinates: Option[Coordinates], fromPage: Long) {
  val latitude: Option[Double] = coordinates.map(_.latitude)
  val longitude: Option[Double] = coordinates.map(_.longitude)
}

object Site extends Infobox[Site] {
  override val infoboxName = "(?:settlement|ancient site)"
  private val nameRegex = infoboxFieldRegex("name")
  private val coordinatesRegex = infoboxCoordinatesRegex("coordinates")

  override def fromInfobox(text: String, fromPage: Long): Option[Site] = {
    val cleanText = cleanInfoboxText(text)

    extractFromRegex(cleanText, nameRegex).map { name =>
      val coordinates = extractFromRegex(cleanText, coordinatesRegex)
      Site(name, coordinates.flatMap(Coordinates.fromTemplate), fromPage)
    }
  }
}