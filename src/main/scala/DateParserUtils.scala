import cats.implicits._
import models.{Date, DateApproximation, DatingLabel, Decade, Year}
import models.DateApproximation.{EarlyVariants, GenericVariants, LateVariants, MiddleVariants}

object DateParserUtils {
  private val ApproximationVariants =
    (EarlyVariants ++ MiddleVariants ++ LateVariants ++ GenericVariants).mkString("|")
  private val DatingLabelVariants =
    (DatingLabel.BCVariants ++ DatingLabel.ADVariants).mkString("|")
  private val DateRegex =
    s"""((?:$ApproximationVariants)\\s+)?([0-9,]+)(s|'s)?\\s*($DatingLabelVariants)?""".r

  def parseDate(dateStr: String): Either[YearParseError, Date] = dateStr.trim match {
    case DateRegex(approximatePrefix, year, decadeSuffix, null) =>
      parseSimpleYear(year).map { parsedYear =>
        (DateApproximation.fromString(approximatePrefix), decadeSuffix) match {
          case (None, null) => Year(parsedYear)
          case (Some(appr), null) => Year(parsedYear, approximation = appr)
          case (None, _) => Decade(parsedYear)
          case (Some(appr), _) => Decade(parsedYear, approximation = appr)
        }
      }
    case DateRegex(approximatePrefix, year, decadeSuffix, label) =>
      DatingLabel.fromString(label).map { datingLabel =>
        val parsedYear = parseSimpleYear(year)
        (DateApproximation.fromString(approximatePrefix), decadeSuffix) match {
          case (None, null) => parsedYear.map(Year(_, datingLabel))
          case (Some(appr), null) => parsedYear.map(Year(_, datingLabel, appr))
          case (None, _) => parsedYear.map(Decade(_, datingLabel))
          case (Some(appr), _) => parsedYear.map(Decade(_, datingLabel, appr))
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
