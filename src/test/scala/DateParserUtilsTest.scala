import cats.implicits._
import models.DatingLabel.{AD, BC}
import models.Year
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class DateParserUtilsTest extends AnyFlatSpec with Matchers {
  import DateParserUtils._

  "The date parser" should "parse a simple year assuming it's AD if nt specified" in {
    parseDate("1000") should ===(Year(1000, AD).asRight)
    parseDate("550") should ===(Year(550, AD).asRight)
    parseDate("12345") should ===(Year(12345, AD).asRight)
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

  it should "allow different formats and ways to specify a year" in {
    parseDate("1 BC") should ===(Year(1, BC).asRight)
    parseDate("2bc") should ===(Year(2, BC).asRight)
    parseDate("3 bc") should ===(Year(3, BC).asRight)
    parseDate("4B.C.") should ===(Year(4, BC).asRight)
    parseDate("5 B.C.") should ===(Year(5, BC).asRight)
    parseDate(" 10 B.C.") should ===(Year(10, BC).asRight)
  }
}
