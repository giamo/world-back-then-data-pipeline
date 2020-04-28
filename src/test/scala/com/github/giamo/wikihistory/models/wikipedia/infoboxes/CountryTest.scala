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
        fromPage = 2000,
        synopsis =
          """The Empire of Japan was the historical nation-state and great power that existed from the Meiji Restoration in 1868 to the enactment of the 1947 constitution of modern Japan.
            |Japan's rapid industrialization and militarization led to its emergence as a world power.""".stripMargin
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
}
