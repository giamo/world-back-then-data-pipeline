import cats.implicits._
import models.{ApproximateDecade, ApproximateYear, Date, DatingLabel, ExactDecade, ExactYear, Year}
import DatingLabel._

object DateParserUtils {
  private val DatingLabelVariants = (DatingLabel.BCVariants ++ DatingLabel.ADVariants).mkString("|")
  private val DateRegex = s"""((?:circa|c\\.|c|ca\\.|ca)\\s+)?([0-9,]+)(s|'s)?\\s*($DatingLabelVariants)?""".r

  def parseDate(dateStr: String): Either[YearParseError, Date] = dateStr.trim match {
    case DateRegex(approximatePrefix, year, decadeSuffix, null) =>
      parseSimpleYear(year).map { parsedYear =>
        (approximatePrefix, decadeSuffix) match {
          case (null, null) => ExactYear(parsedYear)
          case (_, null) => ApproximateYear(parsedYear)
          case (null, _) => ExactDecade(parsedYear)
          case (_, _) => ApproximateDecade(parsedYear)
        }
      }
    case DateRegex(approximatePrefix, year, decadeSuffix, label) =>
      DatingLabel.fromString(label).map { datingLabel =>
        val parsedYear = parseSimpleYear(year)
        (approximatePrefix, decadeSuffix) match {
          case (null, null) => parsedYear.map(ExactYear(_, datingLabel))
          case (_, null) => parsedYear.map(ApproximateYear(_, datingLabel))
          case (null, _) => parsedYear.map(ExactDecade(_, datingLabel))
          case (_, _) => parsedYear.map(ApproximateDecade(_, datingLabel))
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
