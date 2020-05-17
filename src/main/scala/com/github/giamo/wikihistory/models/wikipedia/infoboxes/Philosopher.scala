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
  private val nameRegex = infoboxFieldRegex("name")
  private val birthDateRegex = infoboxFieldRegex("birth_date")
  private val birthPlaceRegex = infoboxFieldRegex("birth_place")
  private val schoolTraditionRegex = infoboxFieldRegex("school_tradition")

  override def fromInfobox(page: WikiPage): Option[Philosopher] = {
    val rawText = page.text

    extractFromRegex(rawText, nameRegex).map { name =>
      val birthDate = extractFromRegex(rawText, birthDateRegex)
      val birthPlace = extractFromRegex(rawText, birthPlaceRegex)
      val schoolTraditions = extractFromRegex(rawText, schoolTraditionRegex)

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
