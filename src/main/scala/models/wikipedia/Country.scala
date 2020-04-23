package models.wikipedia

import models.Date

import scala.util.matching.Regex

final case class Country(
  conventionalName: String,
  name: Option[String],
  yearStart: Option[String],
  yearEnd: Option[String],
  capital: Option[String]
) {
  val parsedYearStart: Option[Int] = yearStart.flatMap(Date.fromString(_).toOption.map(_.toYear))
  val parsedYearEnd: Option[Int] = yearEnd.flatMap(Date.fromString(_).toOption.map(_.toYear))

  val toDateRangeString: Option[String] = for {
    s <- yearStart
    e <- yearEnd
  } yield s"$s - $e"
}

object Country extends Infobox[Country] {
  override val infoboxName = "former country"
  private val nameRegex = infoboxFieldRegex("conventional_long_name")
  private val commonNameRegex = infoboxFieldRegex("common_name")
  private val yearStartRegex = infoboxFieldRegex("year_start")
  private val yearEndRegex = infoboxFieldRegex("year_end")

  // TODO: sometimes there's a list of capitals
  private val capitalRegex =
    ("\\{\\{Infobox " + infoboxName + ".*?capital[\\s]*=[\\s\\[]*([^<${|\\]]+)").r

  override def fromInfobox(text: String): Option[Country] = {
    val cleanText = cleanInfoboxText(text)

    extractFromRegex(cleanText, nameRegex).map { conventionalName =>
      val commonName = extractFromRegex(cleanText, commonNameRegex)
      val yearStart = extractFromRegex(cleanText, yearStartRegex)
      val yearEnd = extractFromRegex(cleanText, yearEndRegex)
      val capital = extractFromRegex(cleanText, capitalRegex)

      Country(
        conventionalName,
        commonName,
        yearStart,
        yearEnd,
        capital
        )
    }
  }
}
