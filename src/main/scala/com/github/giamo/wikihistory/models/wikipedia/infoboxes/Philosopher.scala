package com.github.giamo.wikihistory.models.wikipedia.infoboxes

final case class Philosopher(
  name: String,
  birthDate: Option[String],
  birthPlace: Option[String],
  schoolTraditions: Option[String],
  fromPage: Long
)

object Philosopher extends Infobox[Philosopher] {
  override val infoboxName = "philosopher"
  private val nameRegex = infoboxLinkRegex("name")
  private val birthDateRegex = infoboxFieldRegex("birth_date")
  private val birthPlaceRegex = infoboxLinkRegex("birth_place")
  private val schoolTraditionRegex = infoboxLinkRegex("school_tradition")

  override def fromInfobox(text: String, fromPage: Long): Option[Philosopher] = {
    val cleanText = cleanInfoboxText(text)

    extractFromRegex(cleanText, nameRegex).map { name =>
      val birthDate = extractFromRegex(cleanText, birthDateRegex)
      val birthPlace = extractFromRegex(cleanText, birthPlaceRegex)
      val schoolTraditions = extractFromRegex(cleanText, schoolTraditionRegex)

      Philosopher(
        extractFromFormattedString(name),
        birthDate,
        birthPlace,
        schoolTraditions,
        fromPage
      )
    }
  }
}
