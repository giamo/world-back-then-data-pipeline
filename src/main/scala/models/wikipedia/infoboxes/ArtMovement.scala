package models.wikipedia.infoboxes

import models.Date

final case class ArtMovement(name: String, yearsactive: Option[String], fromPage: Long) {
  val parsedYearsActive = yearsactive.flatMap(Date.fromString(_).toOption)
}

object ArtMovement extends Infobox[ArtMovement] {
  override val infoboxName = "art movement"
  private val nameRegex = infoboxFieldRegex("name")
  private val yearsRegex = infoboxFieldRegex("yearsactive")

  override def fromInfobox(text: String, fromPage: Long): Option[ArtMovement] = {
    val cleanText = cleanInfoboxText(text)

    extractFromRegex(cleanText, nameRegex).map { name =>
      val yearsActive = extractFromRegex(cleanText, yearsRegex)
      ArtMovement(name, yearsActive, fromPage)
    }
  }
}
