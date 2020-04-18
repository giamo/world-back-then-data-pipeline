import cats.implicits._
import models.{DatingLabel, Year}
import DatingLabel._

object DateParserUtils {
  private val DatingLabelVariants = (DatingLabel.BCVariants ++ DatingLabel.ADVariants).mkString("|")
  private val DateRegex = s"""([0-9]+)[\\s]*($DatingLabelVariants)?""".r

  def parseDate(dateStr: String): Either[YearParseError, Year] = dateStr.trim match {
    case DateRegex(year, null) => parseSimpleYear(year).map(Year(_))
    case DateRegex(year, label) =>
      DatingLabel.fromString(label).map { datingLabel =>
        parseSimpleYear(year).map(Year(_, datingLabel))
      }.getOrElse(YearParseError(s"invalid dating label: $label").asLeft)
    case _ => YearParseError(s"invalid date string: '$dateStr'").asLeft
  }

  private def parseSimpleYear(yearStr: String): Either[YearParseError, Int] = yearStr.toInt match {
    case year if year > 0 => year.asRight
    case year => YearParseError(s"$year is not a valid year (must be positive)").asLeft
  }

  final case class YearParseError(message: String)

}
