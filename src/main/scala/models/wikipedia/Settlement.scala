package models.wikipedia

final case class Settlement(name: String, coordinates: Option[String])

object Settlement extends Infobox[Settlement] {
  override val infoboxName = "settlement"
  private val nameRegex = infoboxFieldRegex("name")
  private val coordinatesRegex = ("\\{\\{Infobox " + infoboxName + ".*?coordinates[\\s]*=[\\s]*([^$]+)").r

  override def fromInfobox(text: String): Option[Settlement] = {
    val cleanText = cleanInfoboxText(text)

    extractFromRegex(cleanText, nameRegex).map { name =>
      val coordinates = extractFromRegex(cleanText, coordinatesRegex)
      Settlement(name, coordinates)
    }
  }
}