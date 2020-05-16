package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import com.github.giamo.wikihistory.models.wikipedia.{PageTitle, WikiPage}

import scala.collection.immutable

final case class Philosopher(
  pageId: Long,
  pageTitle: String,
  name: String,
  birthDate: Option[String],
  birthPlace: Option[String],
  schoolTraditions: Option[String]
)

object Philosopher extends Infobox[Philosopher] {
  override val infoboxName = "philosopher"
  private val nameRegex = infoboxLinkRegex("name")
  private val birthDateRegex = infoboxFieldRegex("birth_date")
  private val birthPlaceRegex = infoboxLinkRegex("birth_place")
  private val schoolTraditionRegex = infoboxLinkRegex("school_tradition")

  override def fromInfobox(page: WikiPage): Option[Philosopher] = {
    val cleanText = cleanInfoboxText(page.text)

    extractFromRegex(cleanText, nameRegex).map { name =>
      val birthDate = extractFromRegex(cleanText, birthDateRegex)
      val birthPlace = extractFromRegex(cleanText, birthPlaceRegex)
      val schoolTraditions = extractFromRegex(cleanText, schoolTraditionRegex)

      Philosopher(
        pageId = page.id,
        pageTitle = page.title,
        name = extractFromFormattedString(name),
        birthDate = birthDate,
        birthPlace = birthPlace,
        schoolTraditions = schoolTraditions
      )
    }
  }

  def parseSchoolTraditions(schoolTraditions: String): List[PageTitle] = schoolTraditions
    .split(",")
    .toList
    .flatMap(PageTitle.fromLink)
}
