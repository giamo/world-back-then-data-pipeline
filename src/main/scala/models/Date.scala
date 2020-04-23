package models

import cats.implicits._
import models.DateApproximation.{DateApproximation, NONE}
import models.DatingLabel.{AD, DatingLabel}
import models.DateApproximation.{
  EarlyVariants,
  GenericVariants,
  LateVariants,
  MiddleVariants
}
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
  private val ApproximationVariants =
    (EarlyVariants ++ MiddleVariants ++ LateVariants ++ GenericVariants)
      .mkString("|")
  private val DatingLabelVariants =
    (DatingLabel.BCVariants ++ DatingLabel.ADVariants).mkString("|")
  private val DateRegex =
    s"""((?:$ApproximationVariants)\\s+)?([0-9,]+)(s|'s)?\\s*($DatingLabelVariants)?""".r
  private val CenturyRegex =
    s"""((?:$ApproximationVariants)\\s+)?([0-9]+)(?:st|nd|rd|th)[\\s]+century\\s*($DatingLabelVariants)?""".r

  def fromString(dateStr: String): Either[ParseError, Date] =
    HtmlUtils.cleanHtmlString(dateStr) match {
      case DateRegex(approximatePrefix, year, decadeSuffix, null) =>
        parseSimpleYear(year).map { parsedYear =>
          (DateApproximation.fromString(approximatePrefix), decadeSuffix) match {
            case (None, null)       => Year(parsedYear)
            case (Some(appr), null) => Year(parsedYear, approximation = appr)
            case (None, _)          => Decade(parsedYear)
            case (Some(appr), _)    => Decade(parsedYear, approximation = appr)
          }
        }
      case DateRegex(approximatePrefix, year, decadeSuffix, label) =>
        DatingLabel
          .fromString(label)
          .map { datingLabel =>
            parseSimpleYear(year).map { parsedYear =>
              (DateApproximation.fromString(approximatePrefix), decadeSuffix) match {
                case (None, null)       => Year(parsedYear, datingLabel)
                case (Some(appr), null) => Year(parsedYear, datingLabel, appr)
                case (None, _)          => Decade(parsedYear, datingLabel)
                case (Some(appr), _)    => Decade(parsedYear, datingLabel, appr)
              }
            }
          }
          .getOrElse(DateParseError(s"invalid dating label: $label").asLeft)
      case CenturyRegex(approximatePrefix, century, null) =>
        parseSimpleCentury(century).map { parsedCentury =>
          DateApproximation.fromString(approximatePrefix) match {
            case None       => Century(parsedCentury)
            case Some(appr) => Century(parsedCentury, approximation = appr)
          }
        }
      case CenturyRegex(approximatePrefix, century, label) =>
        DatingLabel
          .fromString(label)
          .map { datingLabel =>
            parseSimpleCentury(century).map { parsedCentury =>
              DateApproximation.fromString(approximatePrefix) match {
                case None => Century(parsedCentury, datingLabel)
                case Some(appr) =>
                  Century(parsedCentury, datingLabel, approximation = appr)
              }
            }
          }
          .getOrElse(DateParseError(s"invalid dating label: $label").asLeft)
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
