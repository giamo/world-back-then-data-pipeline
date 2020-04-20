import cats.implicits._
import models.DateApproximation.{EARLY, GENERIC, LATE, MIDDLE}
import models.DatingLabel.{AD, BC}
import models.{Decade, Year}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class DateParserUtilsTest extends AnyFlatSpec with Matchers {
  import DateParserUtils._

  "The date parser" should "parse a simple year assuming it's AD if nt specified" in {
    parseDate("1000") should ===(Year(1000, AD).asRight)
    parseDate("550") should ===(Year(550, AD).asRight)
    parseDate("12345") should ===(Year(12345, AD).asRight)
    parseDate("01") should ===(Year(1, AD).asRight)
  }

  it should "not allow non-positive years to be parsed" in {
    parseDate("0") should ===(YearParseError("0 is not a valid year (must be positive)").asLeft)
    parseDate("-10") should ===(YearParseError("invalid date string: '-10'").asLeft)
  }

  it should "distinguish between BC and AD dates" in {
    parseDate("1BC") should ===(Year(1, BC).asRight)
    parseDate("1AD") should ===(Year(1, AD).asRight)
    parseDate("1492BC") should ===(Year(1492, BC).asRight)
    parseDate("1492AD") should ===(Year(1492, AD).asRight)
  }

  it should "allow the ',' separator for the thousands" in {
    parseDate("1,500") should ===(Year(1500, AD).asRight)
    parseDate("2,100 BC") should ===(Year(2100, BC).asRight)
    parseDate("1,700,100 BC") should ===(Year(1700100, BC).asRight)
  }

  it should "return an error for non-integer numbers" in {
    parseDate("1.500 BC") should ===(YearParseError("invalid date string: '1.500 BC'").asLeft)
    parseDate("1e2 BC") should ===(YearParseError("invalid date string: '1e2 BC'").asLeft)
  }

  it should "allow different formats and ways to specify labels" in {
    parseDate("1 BC") should ===(Year(1, BC).asRight)
    parseDate("1 AD") should ===(Year(1, AD).asRight)
    parseDate("2bc") should ===(Year(2, BC).asRight)
    parseDate("2ad") should ===(Year(2, AD).asRight)
    parseDate("3 bc") should ===(Year(3, BC).asRight)
    parseDate("3 ad") should ===(Year(3, AD).asRight)
    parseDate("4B.C.") should ===(Year(4, BC).asRight)
    parseDate("4A.D.") should ===(Year(4, AD).asRight)
    parseDate("5 B.C.") should ===(Year(5, BC).asRight)
    parseDate("5 A.D.") should ===(Year(5, AD).asRight)
    parseDate(" 6 B.C.") should ===(Year(6, BC).asRight)
    parseDate(" 6 A.D.") should ===(Year(6, AD).asRight)
    parseDate("7b.c.") should ===(Year(7, BC).asRight)
    parseDate("7a.d.") should ===(Year(7, AD).asRight)
    parseDate("8 b.c.") should ===(Year(8, BC).asRight)
    parseDate("8 a.d.") should ===(Year(8, AD).asRight)
    parseDate("9BCE") should ===(Year(9, BC).asRight)
    parseDate("9CE") should ===(Year(9, AD).asRight)
    parseDate("10 BCE") should ===(Year(10, BC).asRight)
    parseDate("10 CE") should ===(Year(10, AD).asRight)
    parseDate("11 B.C.E.") should ===(Year(11, BC).asRight)
    parseDate("11 C.E.") should ===(Year(11, AD).asRight)
    parseDate("12 b.c.e.") should ===(Year(12, BC).asRight)
    parseDate("12 c.e.") should ===(Year(12, AD).asRight)
  }

  it should "detect the valid formats to specify generically approximate years" in {
    // valid formats
    parseDate("circa 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    parseDate("c. 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    parseDate("c 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    parseDate("ca. 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    parseDate("ca 1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)

    // invalid formats
    parseDate("circa1000 BC") should ===(YearParseError("invalid date string: 'circa1000 BC'").asLeft)
    parseDate("c.1000 BC") should ===(YearParseError("invalid date string: 'c.1000 BC'").asLeft)
    parseDate("c1000 BC") should ===(YearParseError("invalid date string: 'c1000 BC'").asLeft)
    parseDate("ca.1000 BC") should ===(YearParseError("invalid date string: 'ca.1000 BC'").asLeft)
    parseDate("ca1000 BC") should ===(YearParseError("invalid date string: 'ca1000 BC'").asLeft)
  }

  it should "detect the specification of a decade" in {
    parseDate("1920s") should ===(Decade(1920, AD).asRight)
    parseDate("1920's") should ===(Decade(1920, AD).asRight)
    parseDate("circa 1920s BC") should ===(Decade(1920, BC, approximation = GENERIC).asRight)
  }

  it should "detect the specification of more specific approximate dates" in {
    parseDate("early 1100") should ===(Year(1100, AD, approximation = EARLY).asRight)
    parseDate("middle 1100") should ===(Year(1100, AD, approximation = MIDDLE).asRight)
    parseDate("late 1100") should ===(Year(1100, AD, approximation = LATE).asRight)
    parseDate("late 1920s") should ===(Decade(1920, AD, approximation = LATE).asRight)
  }
}
