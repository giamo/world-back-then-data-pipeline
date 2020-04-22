package models

import cats.implicits._
import models.DateApproximation.{EARLY, LATE}
import models.DatingLabel.BC
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class DateRangeTest extends AnyFlatSpec with Matchers {

  import DateRange._

  "The date range parser" should "be constructed by two hyphen-separated dates" in {
    fromString("1980-1990") should ===(
      DateRange(from = Year(1980), to = Year(1990)).asRight
    )
    fromString("100 - 150") should ===(
      DateRange(from = Year(100), to = Year(150)).asRight
    )
    fromString("early 1920s - late 1920s") should ===(
      DateRange(
        from = Decade(1920, approximation = EARLY),
        to = Decade(1920, approximation = LATE)
      ).asRight
    )
    fromString("100 BC -- 50 BC") should ===(
      DateRange(from = Year(100, BC), to = Year(50, BC)).asRight
    )
  }

  it should "raise an error if more than two dates appear" in {
    fromString("1980-1990-2000") should ===(
      DateRangeParseError("invalid date range string: '1980-1990-2000'").asLeft
    )
    fromString("1000 BC - 900 BC - 500 BC") should ===(
      DateRangeParseError(
        "invalid date range string: '1000 BC - 900 BC - 500 BC'"
      ).asLeft
    )
  }

  it should "work in the presence of HTML special characters" in {
    fromString("100BC&nbsp;-&nbsp;50BC") should ===(
      DateRange(from = Year(100, BC), to = Year(50, BC)).asRight
    )
    fromString(
      "&nbsp;early 2008&nbsp;&nbsp;&nbsp;&ndash;&nbsp;&nbsp;late&nbsp;2010&nbsp;"
    ) should ===(
      DateRange(
        from = Year(2008, approximation = EARLY),
        to = Year(2010, approximation = LATE)
      ).asRight
    )
  }
}
