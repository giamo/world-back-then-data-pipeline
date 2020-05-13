package com.github.giamo.wikihistory.models

import cats.implicits._
import com.github.giamo.wikihistory.models.DateApproximation.{AFTER, BEFORE, EARLY, GENERIC, LATE, MIDDLE}
import com.github.giamo.wikihistory.models.DatingLabel.{AD, BC}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class DateTest extends AnyFlatSpec with Matchers {

  "The date parser" should "parse a simple year assuming it's AD if not specified" in {
    Date.fromString("1000") should ===(Year(1000, AD).asRight)
    Date.fromString("550") should ===(Year(550, AD).asRight)
    Date.fromString("12345") should ===(Year(12345, AD).asRight)
    Date.fromString("01") should ===(Year(1, AD).asRight)
  }

  it should "not allow non-positive years to be parsed" in {
    Date.fromString("0") should ===(
      DateParseError("0 is not a valid year (must be a positive integer)").asLeft
    )
    Date.fromString("-10") should ===(
      DateParseError("invalid date string: '-10'").asLeft
    )
  }

  it should "distinguish between BC and AD dates" in {
    Date.fromString("1BC") should ===(Year(1, BC).asRight)
    Date.fromString("1AD") should ===(Year(1, AD).asRight)
    Date.fromString("1492BC") should ===(Year(1492, BC).asRight)
    Date.fromString("1492AD") should ===(Year(1492, AD).asRight)
  }

  it should "allow the ',' separator for the thousands" in {
    Date.fromString("1,500") should ===(Year(1500, AD).asRight)
    Date.fromString("2,100 BC") should ===(Year(2100, BC).asRight)
    Date.fromString("1,700,100 BC") should ===(Year(1700100, BC).asRight)
  }

  it should "return an error for non-integer numbers" in {
    Date.fromString("1.500 BC") should ===(
      DateParseError("invalid date string: '1.500 BC'").asLeft
    )
    Date.fromString("1e2 BC") should ===(
      DateParseError("invalid date string: '1e2 BC'").asLeft
    )
  }

  it should "allow different formats and ways to specify labels" in {
    Date.fromString("1 BC") should ===(Year(1, BC).asRight)
    Date.fromString("1 AD") should ===(Year(1, AD).asRight)
    Date.fromString("2bc") should ===(Year(2, BC).asRight)
    Date.fromString("2ad") should ===(Year(2, AD).asRight)
    Date.fromString("3 bc") should ===(Year(3, BC).asRight)
    Date.fromString("3 ad") should ===(Year(3, AD).asRight)
    Date.fromString("4B.C.") should ===(Year(4, BC).asRight)
    Date.fromString("4A.D.") should ===(Year(4, AD).asRight)
    Date.fromString("5 B.C.") should ===(Year(5, BC).asRight)
    Date.fromString("5 A.D.") should ===(Year(5, AD).asRight)
    Date.fromString(" 6 B.C.") should ===(Year(6, BC).asRight)
    Date.fromString(" 6 A.D.") should ===(Year(6, AD).asRight)
    Date.fromString("7b.c.") should ===(Year(7, BC).asRight)
    Date.fromString("7a.d.") should ===(Year(7, AD).asRight)
    Date.fromString("8 b.c.") should ===(Year(8, BC).asRight)
    Date.fromString("8 a.d.") should ===(Year(8, AD).asRight)
    Date.fromString("9BCE") should ===(Year(9, BC).asRight)
    Date.fromString("9CE") should ===(Year(9, AD).asRight)
    Date.fromString("10 BCE") should ===(Year(10, BC).asRight)
    Date.fromString("10 CE") should ===(Year(10, AD).asRight)
    Date.fromString("11 B.C.E.") should ===(Year(11, BC).asRight)
    Date.fromString("11 C.E.") should ===(Year(11, AD).asRight)
    Date.fromString("12 b.c.e.") should ===(Year(12, BC).asRight)
    Date.fromString("12 c.e.") should ===(Year(12, AD).asRight)
    Date.fromString("13 BC.") should ===(Year(13, BC).asRight)
    Date.fromString("13 CE.") should ===(Year(13, AD).asRight)
  }

  it should "detect the valid formats to specify generically approximate years" in {
    // valid formats
    Date.fromString("circa 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    Date.fromString("c. 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    Date.fromString("c 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    Date.fromString("ca. 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    Date.fromString("ca 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    Date.fromString("''circa'' 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    Date.fromString("{{circa}} 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    Date.fromString("~1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)

    Date.fromString("c1257") should ===(Year(1257, AD, GENERIC).asRight)
    Date.fromString("circa1000 BC") should ===(Year(1000, BC, GENERIC).asRight)
    Date.fromString("c.1000 BC") should ===(Year(1000, BC, GENERIC).asRight)
    Date.fromString("c1000 BC") should ===(Year(1000, BC, GENERIC).asRight)
    Date.fromString("ca.1000 BC") should ===(Year(1000, BC, GENERIC).asRight)
    Date.fromString("ca1000 BC") should ===(Year(1000, BC, GENERIC).asRight)

    // invalid formats
    Date.fromString("*circa* 1000 BC") should ===(DateParseError("invalid date string: '*circa* 1000 BC'").asLeft)
    Date.fromString("something 1000 BC") should ===(DateParseError("invalid date string: 'something 1000 BC'").asLeft)
  }

  it should "detect the specification of a decade" in {
    Date.fromString("1920s") should ===(Decade(1920, AD).asRight)
    Date.fromString("1920's") should ===(Decade(1920, AD).asRight)
    Date.fromString("circa 1920s BC") should ===(
      Decade(1920, BC, approximation = GENERIC).asRight
    )
  }

  it should "detect the specification of more specific approximate dates" in {
    Date.fromString("early 1100") should ===(Year(1100, AD, approximation = EARLY).asRight)
    Date.fromString("middle 1100") should ===(Year(1100, AD, approximation = MIDDLE).asRight)
    Date.fromString("mid 500") should ===(Year(500, AD, approximation = MIDDLE).asRight)
    Date.fromString("mid-500") should ===(Year(500, AD, approximation = MIDDLE).asRight)
    Date.fromString("late 1100") should ===(Year(1100, AD, approximation = LATE).asRight)
    Date.fromString("late 1920s") should ===(Decade(1920, AD, approximation = LATE).asRight)
    Date.fromString("before 1850") should ===(Year(1850, AD, approximation = BEFORE).asRight)
    Date.fromString("after 200 BC") should ===(Year(200, BC, approximation = AFTER).asRight)
  }

  it should "work in the presence of HTML special characters" in {
    Date.fromString("1000&nbsp;BC") should ===(Year(1000, BC).asRight)
    Date.fromString("2000&nbsp;&nbsp;AD") should ===(Year(2000).asRight)
  }

  it should "recognize century dates" in {
    Date.fromString("18th century") should ===(Century(18, AD).asRight)
    Date.fromString("the 18th century") should ===(Century(18, AD).asRight)
    Date.fromString("the early 18th century") should ===(Century(18, AD, approximation = EARLY).asRight)
    Date.fromString("1st century BC") should ===(Century(1, BC).asRight)
    Date.fromString("early 2nd century") should ===(Century(2, AD, approximation = EARLY).asRight)
    Date.fromString("ca. 20th century CE") should ===(Century(20, AD, approximation = GENERIC).asRight)
    Date.fromString("mid-20th century CE") should ===(Century(20, AD, approximation = MIDDLE).asRight)
  }

  it should "detect a year also when month and/or day are present" in {
    Date.fromString("8 February 266") should ===(Year(266, AD).asRight)
    Date.fromString("10 July 420 BC") should ===(Year(420, BC).asRight)
    Date.fromString("december 2000") should ===(Year(2000, AD).asRight)
    Date.fromString("15th august 1900") should ===(Year(1900, AD).asRight)
  }

  it should "ignore any prefix of suffix parenthesis" in {
    Date.fromString("(early 1100)") should ===(Year(1100, AD, approximation = EARLY).asRight)
    Date.fromString("(1000&nbsp;BC)") should ===(Year(1000, BC).asRight)
    Date.fromString("(18th century)") should ===(Century(18, AD).asRight)
    Date.fromString("(8 February 266)") should ===(Year(266, AD).asRight)
  }

  it should "allow the specification of uncertainty between two or more dates (either/or/or)" in {
    Date.fromString("107/108") should ===(UncertainYear(List(Year(107), Year(108))).asRight)
    Date.fromString("107 / 108") should ===(UncertainYear(List(Year(107), Year(108))).asRight)
    Date.fromString("450 / 440 BC") should ===(UncertainYear(List(Year(450, BC), Year(440, BC))).asRight)
    Date.fromString("450 or 440 BC") should ===(UncertainYear(List(Year(450, BC), Year(440, BC))).asRight)
    Date.fromString("either 450 or 440 or 430 BC") should ===(UncertainYear(List(Year(450, BC), Year(440, BC), Year(430, BC))).asRight)
    Date.fromString("428/427 or 424/423 BC") should ===(UncertainYear(List(Year(428, BC), Year(427, BC), Year(424, BC), Year(423, BC))).asRight)
  }

  "Parsing a date range" should "recognize a range of two hyphen- or tilde-separated dates" in {
    Date.fromString("1980-1990") should ===(DateRange(from = Year(1980), to = Year(1990)).asRight)
    Date.fromString("1980~1990") should ===(DateRange(from = Year(1980), to = Year(1990)).asRight)
    Date.fromString("100 - 150") should ===(DateRange(from = Year(100), to = Year(150)).asRight)
    Date.fromString("100 ~ 150") should ===(DateRange(from = Year(100), to = Year(150)).asRight)
    Date.fromString("early 1920s - late 1920s") should ===(
      DateRange(
        from = Decade(1920, approximation = EARLY),
        to = Decade(1920, approximation = LATE)
      ).asRight
    )
    Date.fromString("early 1920s ~ late 1920s") should ===(
      DateRange(
        from = Decade(1920, approximation = EARLY),
        to = Decade(1920, approximation = LATE)
      ).asRight
    )
    Date.fromString("c. 8,000 – c. 2,700 BC") should ===(
      DateRange(
        from = Year(8000, BC, approximation = GENERIC),
        to = Year(2700, BC, approximation = GENERIC)
      ).asRight
    )
    Date.fromString("''circa'' 450 BCE. — ''circa'' 1 BCE     ") should ===(
      DateRange(
        from = Year(450, BC, approximation = GENERIC),
        to = Year(1, BC, approximation = GENERIC)
      ).asRight
    )
  }

  it should "recognize other formats to specify a range" in {
    Date.fromString("from 100 to 50 BC") should ===(DateRange(from = Year(100, BC), to = Year(50, BC)).asRight)
    Date.fromString("5,440 BCE to 460 CE") should ===(DateRange(from = Year(5440, BC), to = Year(460, AD)).asRight)
  }

  it should "raise an error if more than two dates appear" in {
    Date.fromString("1980-1990-2000") should ===(DateParseError("invalid date string: '1980-1990-2000'").asLeft)
    Date.fromString("1000 BC - 900 BC - 500 BC") should ===(DateParseError("invalid date string: '1000 BC - 900 BC - 500 BC'").asLeft)
  }

  it should "work in the presence of HTML special characters" in {
    Date.fromString("100BC&nbsp;-&nbsp;50BC") should ===(DateRange(from = Year(100, BC), to = Year(50, BC)).asRight)
    Date.fromString(
      "&nbsp;early 2008&nbsp;&nbsp;&nbsp;&ndash;&nbsp;&nbsp;late&nbsp;2010&nbsp;"
    ) should ===(
      DateRange(
        from = Year(2008, approximation = EARLY),
        to = Year(2010, approximation = LATE)
      ).asRight
    )
  }

  it should "correctly infer date approximation labels referring to both ends of the range" in {
    Date.fromString("2500~2000 BC") should ===(DateRange(from = Year(2500, BC), to = Year(2000, BC)).asRight)
    Date.fromString("1500 - 1600 AD") should ===(DateRange(from = Year(1500, AD), to = Year(1600, AD)).asRight)
    Date.fromString("100 BC - 50 AD") should ===(DateRange(from = Year(100, BC), to = Year(50, AD)).asRight)
    Date.fromString("100 BC - 50") should ===(DateRange(from = Year(100, BC), to = Year(50, AD)).asRight)
    Date.fromString("150 BC - 100BC") should ===(DateRange(from = Year(150, BC), to = Year(100, BC)).asRight)
  }

  it should "ignore any prefix of suffix parenthesis" in {
    Date.fromString("(1980-1990)") should ===(DateRange(from = Year(1980), to = Year(1990)).asRight)
    Date.fromString("(100BC&nbsp;-&nbsp;50BC)") should ===(DateRange(from = Year(100, BC), to = Year(50, BC)).asRight)
    Date.fromString("(2500~2000 BC)") should ===(DateRange(from = Year(2500, BC), to = Year(2000, BC)).asRight)
  }
}
