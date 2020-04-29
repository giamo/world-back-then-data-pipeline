package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.giamo.wikihistory.UnitTestUtils.readFromFile
import com.github.giamo.wikihistory.models.wikipedia.Coordinates


final class CountryTest extends AnyFlatSpec with Matchers {

  "A country" should "be parsed from a 'country' infobox" in {
    val text = readFromFile("test_pages/country")

    Country.fromInfobox(text, 1000) should ===(
      Country(
        conventionalName = "Colony of the Gold Coast",
        name = "Afriyie Boamah".some,
        yearStart = "1867".some,
        yearEnd = "1957".some,
        fromPage = 1000
      ).some
    )
  }

  it should "be parsed from a 'former country' infobox" in {
    val text = readFromFile("test_pages/former_country")

    Country.fromInfobox(text, 2000) should ===(
      Country(
        conventionalName = "Empire of Great Japan",
        name = "Empire of Japan".some,
        yearStart = "1868&lt;ref&gt;''One can date the &quot;restoration&quot; of imperial rule from the edict of January 3, 1868.'' Jansen, p.334.&lt;/ref&gt;".some,
        yearEnd = "1947&lt;ref name=ndlconstitution/&gt;".some,
        fromPage = 2000,
        coordinates = Coordinates(-55.667, 22.333).some,
        synopsis =
          """The Empire of Japan was the historical nation-state and great power that existed from the Meiji Restoration in 1868 to the enactment of the 1947 constitution of modern Japan.
            |Japan's rapid industrialization and militarization led to its emergence as a world power.""".stripMargin
      ).some
    )
  }

  it should "be parsed from a 'former subdivision' infobox" in {
    val text = readFromFile("test_pages/former_subdivision")

    Country.fromInfobox(text, 2000) should ===(
      Country(
        conventionalName = "Bactria",
        name = "Bactria".some,
        yearStart = "(2500~2000 BC)".some,
        yearEnd = "(900~1000 AD)".some,
        capital = "[[Bactra]]".some,
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

  it should "extract the first paragraph as synopsis" in {
    val text = readFromFile("test_pages/country_with_description")

    Country.fromInfobox(text, 2000) should ===(
      Country(
        conventionalName = "Turkmen Soviet Socialist Republic",
        name = "Turkmen Soviet Socialist Republic".some,
        yearStart = "1925".some,
        yearEnd = "1991".some,
        capital = "[[Ashgabat|Ashkhabad]]".some,
        fromPage = 2000,
        synopsis =
          """
            |The Turkmen Soviet Socialist Republic, also commonly known as Turkmenistan or Turkmenia, was one of the constituent republics of the Soviet Union located in Central Asia existed as a republic from 1925 to 1991. Initially, on 7 August 1921, it was established as the Turkmen Oblast of the Turkestan ASSR before being made, on 13 May 1925, a separate republic of the USSR as the Turkmen SSR.
            |Since then the borders of the Turkmenia were unchanged. On 22 August 1990, Turkmenia declared its sovereignty over Soviet laws. On 27 October 1991, it became independent as the Republic of Turkmenistan.
            |""".stripMargin.trim
      ).some
    )
  }

  it should "contain coordinates even if defined outside of the infobox" in {
    val text = readFromFile("test_pages/country_with_coordinates_outside")

    Country.fromInfobox(text, 1000) should ===(
      Country(
        conventionalName = "Colony of the Gold Coast",
        name = Some("Afriyie Boamah"),
        yearStart = Some("1867"),
        yearEnd = Some("1957"),
        fromPage = 1000,
        coordinates = Coordinates(-55.667, 22.333).some
      ).some
    )
  }

  it should "be parsed from the right infoboxes regardless of case-sensitivity)" in {
    val text1 = "{{Infobox Country\n| CONVENTIONAL_LONG_NAME = Italy\n|}}"
    val text2 = "{{Infobox Former Country\n| conventional_long_name = Sparta\n|}}"
    val text3 = "{{Infobox Former Subdivision\n| Conventional_long_name = Bactria\n|}}"

    Country.fromInfobox(text1, 1) should ===(Country(conventionalName = "Italy", fromPage = 1).some)
    Country.fromInfobox(text2, 2) should ===(Country(conventionalName = "Sparta", fromPage = 2).some)
    Country.fromInfobox(text3, 3) should ===(Country(conventionalName = "Bactria", fromPage = 3).some)
  }

}
