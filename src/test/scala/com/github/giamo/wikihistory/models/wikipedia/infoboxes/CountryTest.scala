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
          """<p>The <b>Empire of Japan</b> was the historical <a href="/wiki/nation-state" title="nation-state">nation-state</a> and <a href="/wiki/great_power" title="great power">great power</a> that existed from the <a href="/wiki/Meiji_Restoration" title="Meiji Restoration">Meiji Restoration</a> in 1868 to the enactment of the <a href="/wiki/Constitution_of_Japan" title="Constitution of Japan">1947 constitution</a> of modern <a href="/wiki/Japan" title="Japan">Japan</a>.<br/>""" +
            """Japan's rapid <a href="/wiki/industrialization" title="industrialization">industrialization</a> and <a href="/wiki/militarization" title="militarization">militarization</a> led to its emergence as a <a href="/wiki/world_power" title="world power">world power</a>.</p>"""
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
          """<p>The <b>Turkmen Soviet Socialist Republic</b>, also commonly known as <b>Turkmenistan</b> or <b>Turkmenia</b>, was one of the <a href="/wiki/republics_of_the_Soviet_Union" title="republics of the Soviet Union">constituent republics</a> of the <a href="/wiki/Soviet_Union" title="Soviet Union">Soviet Union</a> located in <a href="/wiki/Soviet_Central_Asia" title="Soviet Central Asia">Central Asia</a> existed as a republic from 1925 to 1991. Initially, on 7 August 1921, it was established as the Turkmen Oblast of the <a href="/wiki/Turkestan_ASSR" title="Turkestan ASSR">Turkestan ASSR</a> before being made, on 13 May 1925, a separate republic of the USSR as the Turkmen SSR.<br/>""" +
            """Since then the borders of the Turkmenia were unchanged. On 22 August 1990, Turkmenia declared its sovereignty over Soviet laws. On 27 October 1991, it became independent as the <b><a href="/wiki/Republic_of_Turkmenistan" title="Republic of Turkmenistan">Republic of Turkmenistan</a></b>.</p>"""
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
