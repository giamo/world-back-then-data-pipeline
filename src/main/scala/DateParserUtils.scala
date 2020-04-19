import cats.implicits._
import models.{ApproximateYear, DatingLabel, ExactYear, Year}
import DatingLabel._

object DateParserUtils {
  private val DatingLabelVariants = (DatingLabel.BCVariants ++ DatingLabel.ADVariants).mkString("|")
  private val DateRegex = s"""((?:circa|c\\.|c|ca\\.|ca)\\s+)?([0-9,]+)\\s*($DatingLabelVariants)?""".r

  def parseDate(dateStr: String): Either[YearParseError, Year] = dateStr.trim match {
    case DateRegex(approximatePrefix, year, null) =>
      parseSimpleYear(year).map { parsedYear =>
        approximatePrefix match {
          case null => ExactYear(parsedYear)
          case _ => ApproximateYear(parsedYear)
        }
      }
    case DateRegex(approximatePrefix, year, label) =>
      DatingLabel.fromString(label).map { datingLabel =>
        approximatePrefix match {
          case null => parseSimpleYear(year).map(ExactYear(_, datingLabel))
          case _ => parseSimpleYear(year).map(ApproximateYear(_, datingLabel))
        }
      }.getOrElse(YearParseError(s"invalid dating label: $label").asLeft)
    case _ => YearParseError(s"invalid date string: '$dateStr'").asLeft
  }

  private def parseSimpleYear(yearStr: String): Either[YearParseError, Int] = cleanNumber(yearStr).toInt match {
    case year if year > 0 => year.asRight
    case year => YearParseError(s"$year is not a valid year (must be positive)").asLeft
  }

  private def cleanNumber(numberStr: String) = numberStr.replaceAll(",", "")

  final case class YearParseError(message: String)

}
