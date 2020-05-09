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
      case Some(DateRange(f, t)) => f.toYear <= from && t.toYear >= to
      case Some(d: SpecificDate) => d.toYear <= from && d.toYear >= to
      case _ => false
    }
  }
}

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
        synopsis = WikiPage.getHtmlSynopsis(page.text),
        region = region,
        dates = dates
      )
    }
  }
}
