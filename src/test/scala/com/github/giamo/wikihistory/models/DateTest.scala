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
    Date.fromString("1739 [[Common Era|CE]]") should ===(Year(1739, AD).asRight)
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
    Date.fromString("[[Circa|c.]] 11th century") should ===(Century(11, approximation = GENERIC).asRight)
    Date.fromString("~1000 BC") should ===(Year(1000, BC, approximation = GENERIC).asRight)
    Date.fromString("est. 700 AD") should ===(Year(700, AD, approximation = GENERIC).asRight)

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

  it should "detect a year also when month and/or day and/or day-range are present (in different formats)" in {
    Date.fromString("8 February 266") should ===(Year(266, AD).asRight)
    Date.fromString("10 July 420 BC") should ===(Year(420, BC).asRight)
    Date.fromString("december 2000") should ===(Year(2000, AD).asRight)
    Date.fromString("15th august 1900") should ===(Year(1900, AD).asRight)
    Date.fromString("September 27, 1960") should ===(Year(1960, AD).asRight)
    Date.fromString("May 7th,1988") should ===(Year(1988, AD).asRight)
    Date.fromString("December 2019") should ===(Year(2019, AD).asRight)
    Date.fromString("January, 1945") should ===(Year(1945, AD).asRight)
    Date.fromString("November , 1273") should ===(Year(1273, AD).asRight)
    Date.fromString("20 December 1852") should ===(Year(1852, AD).asRight)
    Date.fromString("c. 20th January 1852") should ===(Year(1852, AD, GENERIC).asRight)
    Date.fromString("20-21 December 1852") should ===(Year(1852, AD).asRight)
    Date.fromString("December 20th - 21st 100 BC") should ===(Year(100, BC).asRight)
    Date.fromString("September 20 1674") should ===(Year(1674, AD).asRight)
    Date.fromString("October 11 1675") should ===(Year(1675, AD).asRight)
    Date.fromString("September 20 - October 11 1676") should ===(Year(1676, AD).asRight)
    Date.fromString("30th July - August 1st 1500") should ===(Year(1500, AD).asRight)
    Date.fromString("July-August 500 BCE") should ===(Year(500, BC).asRight)
    Date.fromString("17 to 19 April 1794") should ===(Year(1794, AD).asRight)
    Date.fromString("27—28 February until 5 March 1800") should ===(Year(1800, AD).asRight)
    Date.fromString("September, 14 - October 4, 1943") should ===(Year(1943, AD).asRight)
    Date.fromString("19/20 April 1941") should ===(Year(1941, AD).asRight)
    Date.fromString("June 17 or June 18, 1832") should ===(Year(1832, AD).asRight)
    Date.fromString("From 3 to 4 February 1820") should ===(Year(1820, AD).asRight)
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
    Date.fromString("1849, 1854 or 1855") should ===(UncertainYear(List(Year(1849), Year(1854), Year(1855))).asRight)
    Date.fromString("1849, 1854") should ===(UncertainYear(List(Year(1849), Year(1854))).asRight)
  }

  it should "support the different versions of wiki date format" in {
    Date.fromString("{{Birth date and age|1950|6|11}}") should ===(Year(1950).asRight)
    Date.fromString("{{birth-date and age|1950|6|11}}") should ===(Year(1950).asRight)
    Date.fromString("{{birth date and age|1950|6|11}}") should ===(Year(1950).asRight)
    Date.fromString("{{Birth date|1951|6|11|df=y}}") should ===(Year(1951).asRight)
    Date.fromString("{{Birth year|1952}}") should ===(Year(1952).asRight)
    Date.fromString("{{birth year and age| 1953}}") should ===(Year(1953).asRight)
    Date.fromString("{{birth date and age|df=yes|1954|1|17}}") should ===(Year(1954).asRight)
    Date.fromString("{{birth date and age|df= yes|1929|01|12}}") should ===(Year(1929).asRight)
    Date.fromString("{{birth date| September 27, 1960}}") should ===(Year(1960).asRight)
    Date.fromString("{{nowrap|{{birth date and age|df=yes|1961|9|15}}}}") should ===(Year(1961).asRight)
    Date.fromString("{{bda|1937|4|25}}") should ===(Year(1937).asRight)
    Date.fromString("{{b-da|3 March 1946}}") should ===(Year(1946).asRight)
    Date.fromString("{{Date|18th February 1963}}") should ===(Year(1963).asRight)
    Date.fromString("{{bya|1956}}") should ===(Year(1956).asRight)
    Date.fromString("{{floruit|87 BC}}") should ===(Year(87, BC).asRight)
  }

  it should "support wiki date formats with approximations" in {
    Date.fromString("{{abbr|c.|circa}} {{birth year and age|1955}}") should ===(Year(1955, approximation = GENERIC).asRight)
    Date.fromString("{{circa|398 BCE}}") should ===(Year(398, BC, approximation = GENERIC).asRight)
//    Date.fromString("{{circa|lk=no}} 1080") should ===(Year(398, BC, approximation = GENERIC).asRight)
  }

  it should "allow wiki text after a date" in {
    Date.fromString("24 October 1924|") should ===(Year(1924).asRight)
    Date.fromString("1718 (baptism 8 May 1719)") should ===(Year(1718).asRight)
    Date.fromString(
      "1936 <!-- {{birth date and age|YYYY|MM|DD}} for living persons, {{dirth date|YYYY|MM|DD}} for deceased -->"
    ) should ===(Year(1936).asRight)
    Date.fromString(
      "{{birth year and age|1972}} <!--Only the year unless the exact date is already WIDELY published, as per [[WP:DOB]]. -->"
    ) should ===(Year(1972).asRight)
    Date.fromString("{{birth year and age|1950}} <!--comment") should ===(Year(1950).asRight)
    Date.fromString("c. 446 BC<ref{{cite book}}") should ===(Year(446, BC, approximation = GENERIC).asRight)
    Date.fromString("c. 760, [[Shiraz]], [[Persia]]") should ===(Year(760, approximation = GENERIC).asRight)
    Date.fromString("1365 [[hijri]]<small>(1945)</small>") should ===(Year(1365).asRight)
    Date.fromString("{{circa|586 BC}}|death_date=600") should ===(Year(586, BC, approximation = GENERIC).asRight)
    Date.fromString("{{circa|494 BC}}<ref>{{Cite book}}") should ===(Year(494, BC, approximation = GENERIC).asRight)
    Date.fromString("1493 or 1494<ref name=birth_date>") should ===(UncertainYear(List(Year(1493), Year(1494))).asRight)
    Date.fromString("1116AD<ref name=\"arabency\">") should ===(Year(1116).asRight)
    Date.fromString("October 24, 1906<ref name=Oxford/>") should ===(Year(1906).asRight)
    Date.fromString("1875<br") should ===(Year(1875).asRight)
    Date.fromString("{{Birth-year|1775}} ({{nobold|}})") should ===(Year(1775).asRight)
    Date.fromString("{{circa|785 BC}}{{cite book |editor1-last=Fisher | editor1-first=Marjorie M. }} ") should ===(Year(785, BC, GENERIC).asRight)
  }

  it should "work in the presence of references" in {
    Date.fromString(
      "674<ref name=\\\"ixt\\\">[http://cdigital.dgb.uanl.mx/la/1080012502_C/1080012503_T2/1080012503_MA.PDF Chavero, A. (Ed.) (1892) ''Obras Históricas'']</ref>"
    ) should ===(Year(674).asRight)
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

  "The parsed from/to years" should "both be the year itself for year dates" in {
    val year1 = Year(1990, AD)
    year1.fromYear should ===(1990)
    year1.toYear should ===(1990)

    val year2 = Year(27, BC)
    year2.fromYear should ===(-27)
    year2.toYear should ===(-27)

    val year3 = UncertainYear(List(Year(1900), Year(1950), Year(20, BC)))
    year3.fromYear should ===(-20)
    year3.toYear should ===(1950)
  }

  it should "be the decade's ends for decade dates" in {
    val decade1 = Decade(1990, AD)
    decade1.fromYear should ===(1990)
    decade1.toYear should ===(1999)

    val decade2 = Decade(150, BC)
    decade2.fromYear should ===(-159)
    decade2.toYear should ===(-150)
  }

  it should "be the century's ends for century dates" in {
    val century1 = Century(2, AD)
    century1.fromYear should ===(100)
    century1.toYear should ===(199)

    val century2 = Century(3, BC)
    century2.fromYear should ===(-299)
    century2.toYear should ===(-200)
  }

  it should "be the ranges's ends for range dates" in {
    val range1 = DateRange(Year(1915), Year(1918))
    range1.fromYear should ===(1915)
    range1.toYear should ===(1918)

    val range2 = DateRange(Year(250, BC), Year(200, BC))
    range2.fromYear should ===(-250)
    range2.toYear should ===(-200)
  }

  "A date" should "have a pretty print representation" in {
    Year(1234, AD).toPrettyString() should ===("1234")
    Year(800, BC, EARLY).toPrettyString() should ===("early 800 BCE")
    Year(2020, AD, MIDDLE).toPrettyString() should ===("middle 2020")
    Year(1900, AD, GENERIC).toPrettyString() should ===("circa 1900")

    UncertainYear(List(Year(1950), Year(1951))).toPrettyString() should ===("1950 or 1951")
    UncertainYear(List(Year(1500, BC), Year(1550, BC))).toPrettyString() should ===("1500 BCE or 1550 BCE")

    Decade(1920, AD).toPrettyString() should === ("1920s")
    Decade(800, BC, GENERIC).toPrettyString() should === ("circa 800s BCE")
    Decade(1200, BC, BEFORE).toPrettyString() should === ("before 1200s BCE")

    Century(20, AD).toPrettyString() should ===("20th century")
    Century(13, BC, AFTER).toPrettyString() should ===("after 13th century BCE")
    Century(1, AD, LATE).toPrettyString() should ===("late 1st century")
    Century(2, BC, GENERIC).toPrettyString() should ===("circa 2nd century BCE")
    Century(11, AD).toPrettyString() should ===("11th century")
    Century(12, BC).toPrettyString() should ===("12th century BCE")

    DateRange(Year(1900, AD, GENERIC), Year(1910, AD, GENERIC)).toPrettyString() should ===("circa 1900 - circa 1910")
    DateRange(Year(10, BC), Year(50, AD)).toPrettyString() should ===("10 BCE - 50")
  }
}
