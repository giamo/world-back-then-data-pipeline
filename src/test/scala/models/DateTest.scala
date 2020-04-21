package models

import cats.implicits._
import models.DateApproximation.{EARLY, GENERIC, LATE, MIDDLE}
import models.DatingLabel.{AD, BC}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class DateTest extends AnyFlatSpec with Matchers {

  "The date parser" should "parse a simple year assuming it's AD if nt specified" in {
    Date.fromString("1000") should ===(Year(1000, AD).asRight)
    Date.fromString("550") should ===(Year(550, AD).asRight)
    Date.fromString("12345") should ===(Year(12345, AD).asRight)
    Date.fromString("01") should ===(Year(1, AD).asRight)
  }

  it should "not allow non-positive years to be parsed" in {
    Date.fromString("0") should ===(DateParseError("0 is not a valid year (must be positive)").asLeft)
    Date.fromString("-10") should ===(DateParseError("invalid date string: '-10'").asLeft)
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
    Date.fromString("1.500 BC") should ===(DateParseError("invalid date string: '1.500 BC'").asLeft)
    Date.fromString("1e2 BC") should ===(DateParseError("invalid date string: '1e2 BC'").asLeft)
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
  }

  it should "detect the valid formats to specify generically approximate years" in {
    // valid formats
    Date.fromString("circa 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    Date.fromString("c. 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    Date.fromString("c 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    Date.fromString("ca. 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    Date.fromString("ca 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)

    // invalid formats
    Date.fromString("circa1000 BC") should ===(DateParseError("invalid date string: 'circa1000 BC'").asLeft)
    Date.fromString("c.1000 BC") should ===(DateParseError("invalid date string: 'c.1000 BC'").asLeft)
    Date.fromString("c1000 BC") should ===(DateParseError("invalid date string: 'c1000 BC'").asLeft)
    Date.fromString("ca.1000 BC") should ===(DateParseError("invalid date string: 'ca.1000 BC'").asLeft)
    Date.fromString("ca1000 BC") should ===(DateParseError("invalid date string: 'ca1000 BC'").asLeft)
  }

  it should "detect the specification of a decade" in {
    Date.fromString("1920s") should ===(Decade(1920, AD).asRight)
    Date.fromString("1920's") should ===(Decade(1920, AD).asRight)
    Date.fromString("circa 1920s BC") should ===(Decade(1920, BC, approximation = GENERIC).asRight)
  }

  it should "detect the specification of more specific approximate dates" in {
    Date.fromString("early 1100") should ===(Year(1100, AD, approximation = EARLY).asRight)
    Date.fromString("middle 1100") should ===(Year(1100, AD, approximation = MIDDLE).asRight)
    Date.fromString("late 1100") should ===(Year(1100, AD, approximation = LATE).asRight)
    Date.fromString("late 1920s") should ===(Decade(1920, AD, approximation = LATE).asRight)
  }
}

