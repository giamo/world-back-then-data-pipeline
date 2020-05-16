package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.Date
import com.github.giamo.wikihistory.models.wikipedia.WikiPage

final case class ArtMovement(
  pageId: Long,
  pageTitle: String,
  name: String,
  yearsActive: Option[String]
) {
  val parsedYearsActive = yearsActive.flatMap(Date.fromString(_).toOption)
}

object ArtMovement extends Infobox[ArtMovement] {
  override val infoboxName = "art movement"
  private val nameRegex = infoboxFieldRegex("name")
  private val yearsRegex = infoboxFieldRegex("yearsactive")

  override def fromInfobox(page: WikiPage): Option[ArtMovement] = {
    val cleanText = cleanInfoboxText(page.text)

    extractFromRegex(cleanText, nameRegex).map { name =>
      val yearsActive = extractFromRegex(cleanText, yearsRegex)
      ArtMovement(
        pageId = page.id,
        pageTitle = page.title,
        name = name,
        yearsActive = yearsActive
      )
    }
  }
}
