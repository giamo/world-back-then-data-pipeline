import cats.implicits._
import models.DatingLabel.{AD, BC}
import models.{ApproximateDecade, ApproximateYear, ExactDecade, ExactYear}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class DateParserUtilsTest extends AnyFlatSpec with Matchers {
  import DateParserUtils._

  "The date parser" should "parse a simple year assuming it's AD if nt specified" in {
    parseDate("1000") should ===(ExactYear(1000, AD).asRight)
    parseDate("550") should ===(ExactYear(550, AD).asRight)
    parseDate("12345") should ===(ExactYear(12345, AD).asRight)
    parseDate("01") should ===(ExactYear(1, AD).asRight)
  }

  it should "not allow non-positive years to be parsed" in {
    parseDate("0") should ===(YearParseError("0 is not a valid year (must be positive)").asLeft)
    parseDate("-10") should ===(YearParseError("invalid date string: '-10'").asLeft)
  }

  it should "distinguish between BC and AD dates" in {
    parseDate("1BC") should ===(ExactYear(1, BC).asRight)
    parseDate("1AD") should ===(ExactYear(1, AD).asRight)
    parseDate("1492BC") should ===(ExactYear(1492, BC).asRight)
    parseDate("1492AD") should ===(ExactYear(1492, AD).asRight)
  }

  it should "allow the ',' separator for the thousands" in {
    parseDate("1,500") should ===(ExactYear(1500, AD).asRight)
    parseDate("2,100 BC") should ===(ExactYear(2100, BC).asRight)
    parseDate("1,700,100 BC") should ===(ExactYear(1700100, BC).asRight)
  }

  it should "return an error for non-integer numbers" in {
    parseDate("1.500 BC") should ===(YearParseError("invalid date string: '1.500 BC'").asLeft)
    parseDate("1e2 BC") should ===(YearParseError("invalid date string: '1e2 BC'").asLeft)
  }

  it should "allow different formats and ways to specify labels" in {
    parseDate("1 BC") should ===(ExactYear(1, BC).asRight)
    parseDate("1 AD") should ===(ExactYear(1, AD).asRight)
    parseDate("2bc") should ===(ExactYear(2, BC).asRight)
    parseDate("2ad") should ===(ExactYear(2, AD).asRight)
    parseDate("3 bc") should ===(ExactYear(3, BC).asRight)
    parseDate("3 ad") should ===(ExactYear(3, AD).asRight)
    parseDate("4B.C.") should ===(ExactYear(4, BC).asRight)
    parseDate("4A.D.") should ===(ExactYear(4, AD).asRight)
    parseDate("5 B.C.") should ===(ExactYear(5, BC).asRight)
    parseDate("5 A.D.") should ===(ExactYear(5, AD).asRight)
    parseDate(" 6 B.C.") should ===(ExactYear(6, BC).asRight)
    parseDate(" 6 A.D.") should ===(ExactYear(6, AD).asRight)
    parseDate("7b.c.") should ===(ExactYear(7, BC).asRight)
    parseDate("7a.d.") should ===(ExactYear(7, AD).asRight)
    parseDate("8 b.c.") should ===(ExactYear(8, BC).asRight)
    parseDate("8 a.d.") should ===(ExactYear(8, AD).asRight)
    parseDate("9BCE") should ===(ExactYear(9, BC).asRight)
    parseDate("9CE") should ===(ExactYear(9, AD).asRight)
    parseDate("10 BCE") should ===(ExactYear(10, BC).asRight)
    parseDate("10 CE") should ===(ExactYear(10, AD).asRight)
    parseDate("11 B.C.E.") should ===(ExactYear(11, BC).asRight)
    parseDate("11 C.E.") should ===(ExactYear(11, AD).asRight)
    parseDate("12 b.c.e.") should ===(ExactYear(12, BC).asRight)
    parseDate("12 c.e.") should ===(ExactYear(12, AD).asRight)
  }

  it should "detect the valid formats to specify approximate years" in {
    // valid formats
    parseDate("circa 1000 BC") should ===(ApproximateYear(1000, BC).asRight)
    parseDate("c. 1000 BC") should ===(ApproximateYear(1000, BC).asRight)
    parseDate("c 1000 BC") should ===(ApproximateYear(1000, BC).asRight)
    parseDate("ca. 1000 BC") should ===(ApproximateYear(1000, BC).asRight)
    parseDate("ca 1000 BC") should ===(ApproximateYear(1000, BC).asRight)

    // invalid formats
    parseDate("circa1000 BC") should ===(YearParseError("invalid date string: 'circa1000 BC'").asLeft)
    parseDate("c.1000 BC") should ===(YearParseError("invalid date string: 'c.1000 BC'").asLeft)
    parseDate("c1000 BC") should ===(YearParseError("invalid date string: 'c1000 BC'").asLeft)
    parseDate("ca.1000 BC") should ===(YearParseError("invalid date string: 'ca.1000 BC'").asLeft)
    parseDate("ca1000 BC") should ===(YearParseError("invalid date string: 'ca1000 BC'").asLeft)
  }

  it should "detect the specification of a decade" in {
    parseDate("1920s") should ===(ExactDecade(1920, AD).asRight)
    parseDate("1920's") should ===(ExactDecade(1920, AD).asRight)
    parseDate("circa 1920s BC") should ===(ApproximateDecade(1920, BC).asRight)
  }
}
