package models

import cats.implicits._

final case class DateRange(from: Date, to: Date)

object DateRange {
  private val DateRangeRegex = "([^-]+)[\\-\\s]+([^-]+)".r

  def fromString(dateRangeStr: String): Either[ParseError, DateRange] = dateRangeStr.trim match {
    case DateRangeRegex(fromStr, toStr) =>
      for {
        from <- Date.fromString(fromStr)
        to <- Date.fromString(toStr)
      } yield DateRange(from, to)
    case _ => DateRangeParseError(s"invalid date range string: '$dateRangeStr'").asLeft

  }
}