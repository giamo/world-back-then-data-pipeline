package models

import cats.implicits._
import models.DateApproximation.{DateApproximation, NONE}
import models.DatingLabel._
import models.DateApproximation._
import utils.HtmlUtils

sealed trait Date {
  val label: DatingLabel
  val approximation: DateApproximation

  def toYear: Int
}

final case class Year(yearNumber: Int,
                      label: DatingLabel = AD,
                      approximation: DateApproximation = NONE)
    extends Date {
  override def toYear: Int = if (label == AD) yearNumber else -1 * yearNumber
}

final case class Decade(decadeNumber: Int,
                        label: DatingLabel = AD,
                        approximation: DateApproximation = NONE)
    extends Date {
  override def toYear: Int =
    if (label == AD) decadeNumber else -1 * decadeNumber
}

final case class Century(centuryNumber: Int,
                         label: DatingLabel = AD,
                         approximation: DateApproximation = NONE)
    extends Date {
  override def toYear: Int =
    100 * (if (label == AD) centuryNumber else -1 * centuryNumber)
}

object Date {
  private val DateRegex =
    s"""((?:$ApproximationVariantsStr)\\s+)?([0-9,]+)(s|'s)?\\s*($DatingLabelVariantsStr)?""".r
  private val CenturyRegex =
    s"""((?:$ApproximationVariantsStr)\\s+)?([0-9]+)(?:st|nd|rd|th)[\\s]+century\\s*($DatingLabelVariantsStr)?""".r

  def fromString(dateStr: String): Either[ParseError, Date] =
    HtmlUtils.cleanHtmlString(dateStr).toLowerCase match {
      case DateRegex(approximatePrefix, year, decadeSuffix, label) =>
        for {
          datingLabel <- DatingLabel.fromString(label)
          approx <- DateApproximation.fromString(approximatePrefix)
          parsedYear <- parseSimpleYear(year)
        } yield {
          decadeSuffix match {
            case null => Year(parsedYear, datingLabel, approx)
            case _    => Decade(parsedYear, datingLabel, approx)
          }
        }
      case CenturyRegex(approximatePrefix, century, label) =>
        for {
          datingLabel <- DatingLabel.fromString(label)
          approx <- DateApproximation.fromString(approximatePrefix)
          parsedCentury <- parseSimpleCentury(century)
        } yield Century(parsedCentury, datingLabel, approx)
      case _ => DateParseError(s"invalid date string: '$dateStr'").asLeft
    }

  private def parseSimpleYear(yearStr: String): Either[DateParseError, Int] =
    cleanNumber(yearStr).toInt match {
      case year if year > 0 => year.asRight
      case _ =>
        DateParseError(
          s"$yearStr is not a valid year (must be a positive integer)"
        ).asLeft
    }

  private def parseSimpleCentury(
    centuryStr: String
  ): Either[DateParseError, Int] =
    centuryStr.toInt match {
      case century if century > 0 => century.asRight
      case _ =>
        DateParseError(
          s"$centuryStr is not a valid century (must be a positive integer)"
        ).asLeft
    }

  private def cleanNumber(numberStr: String) = numberStr.replaceAll(",", "")

}
