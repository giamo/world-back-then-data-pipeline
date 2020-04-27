package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.github.giamo.wikihistory.UnitTestUtils.readFromFile


final class CountryTest extends AnyFlatSpec with Matchers {

  "A country" should "be parsed from a 'country' infobox" in {
    val text = readFromFile("test_pages/country")

    Country.fromInfobox(text, 1000) should ===(
      Country(
        conventionalName = "Colony of the Gold Coast",
        name = Some("Afriyie Boamah"),
        yearStart = Some("1867"),
        yearEnd = Some("1957"),
        fromPage = 1000
      ).some
    )
  }

  it should "be parsed from a 'former country' infobox" in {
    val text = readFromFile("test_pages/former_country")

    Country.fromInfobox(text, 2000) should ===(
      Country(
        conventionalName = "Empire of Great Japan",
        name = Some("Empire of Japan"),
        yearStart = Some("1868&lt;ref&gt;''One can date the &quot;restoration&quot; of imperial rule from the edict of January 3, 1868.'' Jansen, p.334.&lt;/ref&gt;"),
        yearEnd = Some("1947&lt;ref name=ndlconstitution/&gt;"),
        fromPage = 2000
      ).some
    )
  }

  it should "extract country names from formatted strings" in {
    val text =
      """{{Infobox former country
        || conventional_long_name = {{raise|0.2em|Xianyang}}
        |}}
        |""".stripMargin

    Country.fromInfobox(text, 2000) should ===(
      Country(conventionalName = "Xianyang", fromPage = 2000).some
    )
  }
}
