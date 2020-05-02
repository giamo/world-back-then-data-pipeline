package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.wikipedia.WikiPage

final case class ArchaeologicalCulture(
  pageId: Long,
  pageTitle: String,
  name: String,
  region: Option[String],
  dates: Option[String]
)

object ArchaeologicalCulture extends Infobox[ArchaeologicalCulture] {
  def apply(s: String): Option[ArchaeologicalCulture] = ???

  override val infoboxName = "archaeological culture"
  private val nameRegex = infoboxFieldRegex("name")
  private val regionRegex = infoboxFieldRegex("region")
  private val datesRegex = infoboxFieldRegex("dates")

  override def fromInfobox(page: WikiPage): Option[ArchaeologicalCulture] = {
    val cleanText = cleanInfoboxText(page.text)

    extractFromRegex(cleanText, nameRegex).map { name =>
      val region = extractFromRegex(cleanText, regionRegex)
      val dates = extractFromRegex(cleanText, datesRegex)

      ArchaeologicalCulture(
        pageId = page.id,
        pageTitle = page.title,
        name = name,
        region = region,
        dates = dates
      )
    }
  }
}
