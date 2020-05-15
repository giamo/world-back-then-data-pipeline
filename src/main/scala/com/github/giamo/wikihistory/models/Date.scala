package com.github.giamo.wikihistory.models

import cats.implicits._
import com.github.giamo.wikihistory.models.DateApproximation.{DateApproximation, NONE}
import com.github.giamo.wikihistory.models.DatingLabel._
import com.github.giamo.wikihistory.models.DateApproximation._
import com.github.giamo.wikihistory.utils.HtmlUtils

import scala.util.{Success, Try}

sealed trait Date {
  def toYear: Int

  def isBefore(other: Date): Boolean = this.toYear < other.toYear
}

sealed trait SpecificDate extends Date {
  val label: DatingLabel
  val approximation: DateApproximation
}

sealed trait UncertainDate extends Date

final case class Year(
  yearNumber: Int,
  label: DatingLabel = AD,
  approximation: DateApproximation = NONE
) extends SpecificDate {
  override def toYear: Int = if (label == AD) yearNumber else -1 * yearNumber
}

final case class UncertainYear(variants: List[Date]) extends UncertainDate {
  override def toYear: Int = variants.map(_.toYear).min
}

final case class Decade(
  decadeNumber: Int,
  label: DatingLabel = AD,
  approximation: DateApproximation = NONE
) extends SpecificDate {
  override def toYear: Int =
    if (label == AD) decadeNumber else -1 * decadeNumber
}

final case class Century(
  centuryNumber: Int,
  label: DatingLabel = AD,
  approximation: DateApproximation = NONE
) extends SpecificDate {
  override def toYear: Int =
    100 * (if (label == AD) centuryNumber else -1 * centuryNumber)
}

final case class DateRange(from: Date, to: Date) extends Date {
  override def toYear: Int = from.toYear
}

object Date {
  private val MonthsVariantsStr =
    List("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
      .mkString("|")
  private val CardinalVariants = List("st", "nd", "rd", "th").mkString("|")
  private val RegexTail = "(?:\\s*(?:\\||\\(.*\\)|<!\\-\\-|<ref|,?\\s*\\[\\[|<br).*)?"

  private val YearRegex =
    s"""\\(?($ApproximationVariantsStr)?\\s*(?:(?:[0-9]+(?:$CardinalVariants)?)?\\s*(?:$MonthsVariantsStr)(?:\\s*,)?|(?:$MonthsVariantsStr)(?:\\s+[0-9]+(?:$CardinalVariants)?)?(?:\\s*,)?)?\\s*([0-9,]+)(s|'s)?\\s*($DatingLabelVariantsStr)?\\)?$RegexTail""".r
  private val CenturyRegex =
    s"""\\(?(?:the\\s+)?($ApproximationVariantsStr)?\\s*([0-9]+)(?:$CardinalVariants)[\\s]+century\\s*($DatingLabelVariantsStr)?\\)?$RegexTail""".r
  private val WikiDateRegex =
    s"(?s)(?:\\{\\{.*($ApproximationVariantsStr).*\\}\\}\\s*)?.*?\\{\\{\\s*(circa|birth|bda|b-da|date|bya|floruit)[a-z\\s\\-]*\\|\\s*(?:[a-z]+\\s*=\\s*[a-z]+\\|\\s*)?([^\\|\\}]+)\\s*(?:\\}\\}|\\|.*)$RegexTail".r
  private val AlternativesRegex =
    s"\\(?(?:either\\s+)?([^-]+?($DatingLabelVariantsStr)?)\\s*(?:or|\\/|,)\\s*([^-]+?($DatingLabelVariantsStr)?)\\)?$RegexTail".r
  private val RangeRegex =
    s"\\(?(?:\\s*from\\s+)?([^-]+?($DatingLabelVariantsStr)?)\\s*(?:\\-|\\~|\\—|\\sto)\\s*([^-]+?($DatingLabelVariantsStr)?)\\)?$RegexTail".r

  // TODO: cover more corner cases
  // ex: 1936 , August 3rd
  // ex: 1988-07-05
  // ex: {{circa|lk=no}} 1080
  // ex: 331/0 BC
  // ex: {{OldStyleDate|1 October|1912|18 September}}
  // ex: c.  AD 20 – 30
  // ex: Unclear, 6th – 4th century BCE
  // ex: 1000 CE / 390 AH
  // ex: {{abbr|c.|circa}} 832
  // ex: (206 [[Islamic calendar|AH]]  (821/2 AD))
  // ex: 3649 KE 779 AH or 1377 CE
  // ex: Unknown (/Uncertain), 6th Century – 4th century BC
  // ex: [[1937]]
  def fromString(dateStr: String): Either[ParseError, Date] =
    HtmlUtils.cleanHtmlString(dateStr).toLowerCase match {
      case YearRegex(approximatePrefix, year, decadeSuffix, label) =>
        for {
          datingLabel <- DatingLabel.fromString(label)
          approx <- DateApproximation.fromString(approximatePrefix)
          parsedYear <- parseSimpleYear(year)
        } yield {
          decadeSuffix match {
            case null => Year(parsedYear, datingLabel, approx)
            case _ => Decade(parsedYear, datingLabel, approx)
          }
        }
      case CenturyRegex(approximatePrefix, century, label) =>
        for {
          datingLabel <- DatingLabel.fromString(label)
          approx <- DateApproximation.fromString(approximatePrefix)
          parsedCentury <- parseSimpleCentury(century)
        } yield Century(parsedCentury, datingLabel, approx)
      case WikiDateRegex(approximatePrefix, wikiPrefix, date) =>
        if (approximatePrefix != null) fromString(s"$approximatePrefix $date")
        else if (GenericVariants.contains(wikiPrefix.toLowerCase)) fromString(s"$wikiPrefix $date")
        else fromString(date)
      case RangeRegex(fromDate, fromLabel, toDate, toLabel) =>
        val updatedFromDate =
          if (fromLabel == null && toLabel != null) s"$fromDate $toLabel"
          else fromDate
        for {
          from <- fromString(updatedFromDate)
          to <- fromString(toDate)
        } yield DateRange(from, to)
      case AlternativesRegex(fromDate, fromLabel, toDate, toLabel) =>
        val updatedFromDate =
          if (fromLabel == null && toLabel != null) s"$fromDate $toLabel"
          else fromDate
        for {
          from <- fromString(updatedFromDate)
          to <- fromString(toDate)
        } yield (from, to) match {
          case (UncertainYear(alts1), UncertainYear(alts2)) => UncertainYear(alts1 ++ alts2)
          case (UncertainYear(alts), d) => UncertainYear(alts :+ d)
          case (d, UncertainYear(alts)) => UncertainYear(List(d) ++ alts)
          case (d1, d2) => UncertainYear(List(d1, d2))
        }
      case _ => DateParseError(s"invalid date string: '$dateStr'").asLeft
    }

  private def parseSimpleYear(yearStr: String): Either[DateParseError, Int] =
    Try(cleanNumber(yearStr).toInt) match {
      case Success(year) if year > 0 => year.asRight
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
