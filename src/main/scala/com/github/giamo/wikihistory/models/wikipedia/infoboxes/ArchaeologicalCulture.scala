package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.{Date, DateRange, SpecificDate}
import com.github.giamo.wikihistory.models.wikipedia.WikiPage

final case class ArchaeologicalCulture(
  pageId: Long,
  pageTitle: String,
  name: String,
  synopsis: String = "",
  region: Option[String],
  dates: Option[String]
) {
  def isIncludedinRange(from: Int, to: Int): Boolean = {
    dates.flatMap(d => Date.fromString(d).toOption) match {
      case Some(DateRange(f, t)) => f.fromYear <= from && t.fromYear >= to
      case Some(d: SpecificDate) => d.fromYear <= from && d.fromYear >= to
      case _ => false
    }
  }
}

object ArchaeologicalCulture extends Infobox[ArchaeologicalCulture] {
  override val infoboxName = "archaeological culture"
  private val nameRegex = infoboxFieldRegex("name")
  private val regionRegex = infoboxFieldRegex("region")
  private val datesRegex = infoboxFieldRegex("dates")

  override def fromInfobox(page: WikiPage): Option[ArchaeologicalCulture] = {
    val rawText = page.text

    extractFromRegex(rawText, nameRegex).map { name =>
      val region = extractFromRegex(rawText, regionRegex)
      val dates = extractFromRegex(rawText, datesRegex)

      ArchaeologicalCulture(
        pageId = page.id,
        pageTitle = page.title,
        name = name,
        synopsis = WikiPage.getCleanHtml(page.text),
        region = region,
        dates = dates
      )
    }
  }
}
