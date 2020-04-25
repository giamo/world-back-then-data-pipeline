package models.wikipedia

final case class ArchaeologicalCulture(
  name: String,
  region: Option[String],
  dates: Option[String],
  fromPage: Long
)

object ArchaeologicalCulture extends Infobox[ArchaeologicalCulture] {
  def apply(s: String): Option[ArchaeologicalCulture] = ???

  override val infoboxName = "archaeological culture"
  private val nameRegex = infoboxFieldRegex("name")
  private val regionRegex = infoboxFieldRegex("region")
  private val datesRegex = infoboxFieldRegex("dates")

  override def fromInfobox(text: String, fromPage: Long): Option[ArchaeologicalCulture] = {
    val cleanText = cleanInfoboxText(text)

    extractFromRegex(cleanText, nameRegex).map { name =>
      val region = extractFromRegex(cleanText, regionRegex)
      val dates = extractFromRegex(cleanText, datesRegex)

      ArchaeologicalCulture(name, region, dates, fromPage)
    }
  }
}
