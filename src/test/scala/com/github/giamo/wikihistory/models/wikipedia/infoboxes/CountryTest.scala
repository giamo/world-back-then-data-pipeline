package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.giamo.wikihistory.UnitTestUtils.readFromFile
import com.github.giamo.wikihistory.models.wikipedia.{Capital, Coordinates}
import com.github.giamo.wikihistory.UnitTestUtils._

final class CountryTest extends AnyFlatSpec with Matchers {

  "A country" should "be parsed from a 'country' infobox" in {
    val text = readFromFile("test_pages/country")
    val testPage = sampleWikiPage(1000, "Gold Coast", text)

    Country.fromInfobox(testPage) should ===(
      Country(
        pageId = 1000,
        pageTitle = "Gold Coast",
        infoboxType = "country",
        conventionalName = "Colony of the Gold Coast",
        name = "Afriyie Boamah".some,
        yearStart = "1867".some,
        yearEnd = "1957".some,
        governmentType = "[[Colonialism|Colonial]]".some,
        imageCoat = "Badge of the Gold Coast (1877-1957).svg".some
      ).some
    )
  }

  it should "be parsed from a 'former country' infobox" in {
    val text = readFromFile("test_pages/former_country")
    val testPage = sampleWikiPage(2000, "Empire of Japan", text)

    Country.fromInfobox(testPage) should ===(
      Country(
        pageId = 2000,
        pageTitle = "Empire of Japan",
        infoboxType = "former country",
        conventionalName = "Empire of Great Japan",
        synopsis =
          """<p>The <b>Empire of Japan</b> was the historical <a target="_blank" href="https://en.wikipedia.org/wiki/nation-state" title="nation-state">nation-state</a> and <a target="_blank" href="https://en.wikipedia.org/wiki/great_power" title="great power">great power</a> that existed from the <a target="_blank" href="https://en.wikipedia.org/wiki/Meiji_Restoration" title="Meiji Restoration">Meiji Restoration</a> in 1868 to the enactment of the <a target="_blank" href="https://en.wikipedia.org/wiki/Constitution_of_Japan" title="Constitution of Japan">1947 constitution</a> of modern <a target="_blank" href="https://en.wikipedia.org/wiki/Japan" title="Japan">Japan</a>.</p>""" +
            """<p>Japan's rapid <a target="_blank" href="https://en.wikipedia.org/wiki/industrialization" title="industrialization">industrialization</a> and <a target="_blank" href="https://en.wikipedia.org/wiki/militarization" title="militarization">militarization</a> led to its emergence as a <a target="_blank" href="https://en.wikipedia.org/wiki/world_power" title="world power">world power</a>.</p>""", name = "Empire of Japan".some, yearStart = "1868&lt;ref&gt;''One can date the &quot;restoration&quot; of imperial rule from the edict of January 3, 1868.'' Jansen, p.334.&lt;/ref&gt;".some, yearEnd = "1947&lt;ref name=ndlconstitution/&gt;".some,
        coordinates = Coordinates(-55.667, 22.333).some,
        imageCoat = "Imperial Seal of Japan.svg".some,
        religion = "Buddhism".some
      ).some
    )
  }

  it should "be parsed from a 'former subdivision' infobox" in {
    val text = readFromFile("test_pages/former_subdivision")
    val testPage = sampleWikiPage(3000, "Bactria", text)

    Country.fromInfobox(testPage) should ===(
      Country(
        pageId = 3000,
        pageTitle = "Bactria",
        infoboxType = "former subdivision",
        conventionalName = "Bactria",
        name = "Bactria".some,
        yearStart = "(2500~2000 BC)".some,
        yearEnd = "(900~1000 AD)".some,
        capital = "[[Bactra]]".some
      ).some
    )
  }

  it should "extract country names from formatted strings" in {
    val text =
      """{{Infobox former country
        || conventional_long_name = {{raise|0.2em|Xianyang}}
        |}}
        |""".stripMargin
    val testPage = sampleWikiPage(4000, "Xianyang (city)", text)

    Country.fromInfobox(testPage) should ===(
      Country(pageId = 4000, pageTitle = "Xianyang (city)", infoboxType = "former country", conventionalName = "Xianyang").some
    )
  }

  it should "extract the first paragraph as synopsis" in {
    val text = readFromFile("test_pages/country_with_description")
    val testPage = sampleWikiPage(5000, "Turkmenia", text)

    Country.fromInfobox(testPage) should ===(
      Country(
        pageId = 5000,
        pageTitle = "Turkmenia",
        infoboxType = "country",
        conventionalName = "Turkmen Soviet Socialist Republic",
        synopsis =
          """<p>The <b>Turkmen Soviet Socialist Republic</b>, also commonly known as <b>Turkmenistan</b> or <b>Turkmenia</b>, was one of the <a target="_blank" href="https://en.wikipedia.org/wiki/republics_of_the_Soviet_Union" title="republics of the Soviet Union">constituent republics</a> of the <a target="_blank" href="https://en.wikipedia.org/wiki/Soviet_Union" title="Soviet Union">Soviet Union</a> located in <a target="_blank" href="https://en.wikipedia.org/wiki/Soviet_Central_Asia" title="Soviet Central Asia">Central Asia</a> existed as a republic from 1925 to 1991. Initially, on 7 August 1921, it was established as the Turkmen Oblast of the <a target="_blank" href="https://en.wikipedia.org/wiki/Turkestan_ASSR" title="Turkestan ASSR">Turkestan ASSR</a> before being made, on 13 May 1925, a separate republic of the USSR as the Turkmen SSR.
            |Since then the borders of the Turkmenia were unchanged. On 22 August 1990, Turkmenia declared its sovereignty over Soviet laws. On 27 October 1991, it became independent as the <b><a target="_blank" href="https://en.wikipedia.org/wiki/Republic_of_Turkmenistan" title="Republic of Turkmenistan">Republic of Turkmenistan</a></b>.</p>""".stripMargin,
        name = "Turkmen Soviet Socialist Republic".some,
        yearStart = "1925".some,
        yearEnd = "1991".some,
        capital = "[[Ashgabat|Ashkhabad]]".some,
        governmentType = "[[Unitary state|Unitary]] [[Marxism-Leninism|Marxist-Leninist]] [[one-party state|single-party]] [[Soviet republic (system of government)|Soviet]] [[socialist state|socialist republic]] (1925–1990)&lt;br&gt;[[Unitary state|Unitary]] [[Presidential system|presidential]] [[republic]] (1990–1991)".some,
        imageCoat = "Emblem of the Turkmen SSR.svg".some,
        commonLanguages = "[[Turkmen language|Turkmen]]{{·}}[[Russian language|Russian]]".some
      ).some
    )
  }

  it should "contain coordinates even if defined outside of the infobox" in {
    val text = readFromFile("test_pages/country_with_coordinates_outside")
    val testPage = sampleWikiPage(1000, "Gold Coast", text)

    Country.fromInfobox(testPage) should ===(
      Country(
        pageId = 1000,
        pageTitle = "Gold Coast",
        infoboxType = "country",
        conventionalName = "Colony of the Gold Coast",
        name = Some("Afriyie Boamah"),
        yearStart = Some("1867"),
        yearEnd = Some("1957"),
        coordinates = Coordinates(-55.667, 22.333).some,
        governmentType = "[[Colonialism|Colonial]]".some,
        imageCoat = "Badge of the Gold Coast (1877-1957).svg".some
      ).some
    )
  }

  it should "be parsed from the right infoboxes regardless of case-sensitivity)" in {
    val text1 = "{{Infobox Country\n| CONVENTIONAL_LONG_NAME = Italy\n|}}"
    val text2 = "{{Infobox Former Country\n| conventional_long_name = Sparta\n|}}"
    val text3 = "{{Infobox Former Subdivision\n| Conventional_long_name = Bactria\n|}}"
    val testPage1 = sampleWikiPage(1, "Italy", text1)
    val testPage2 = sampleWikiPage(2, "Sparta", text2)
    val testPage3 = sampleWikiPage(3, "Bactria", text3)

    Country.fromInfobox(testPage1) should ===(Country(pageId = 1, pageTitle = "Italy", infoboxType = "country", conventionalName = "Italy").some)
    Country.fromInfobox(testPage2) should ===(Country(pageId = 2, pageTitle = "Sparta", infoboxType = "former country", conventionalName = "Sparta").some)
    Country.fromInfobox(testPage3) should ===(Country(pageId = 3, pageTitle = "Bactria", infoboxType = "former subdivision", conventionalName = "Bactria").some)
  }

  it should "parse all the relevant text in a field, regardless of newlines" in {
    val text =
      """
        |{{Infobox country
        ||conventional_long_name = Rome
        ||capital                = {{plainlist|
        |* [[Seleucia]]{{small|&lt;br/&gt;(305–240 BC)}}
        |* [[Antioch]]{{small|&lt;br/&gt;(240–63 BC)}}}}
        ||other = value
        |}}
        |""".stripMargin
    val testPage = sampleWikiPage(1, "Rome", text)
    Country.fromInfobox(testPage) should ===(
      Country(
        pageId = 1,
        pageTitle = "Rome",
        infoboxType = "country",
        conventionalName = "Rome",
        capital = "{{plainlist|\n* [[Seleucia]]{{small|&lt;br/&gt;(305–240 BC)}}\n* [[Antioch]]{{small|&lt;br/&gt;(240–63 BC)}}}}".some
      ).some)
  }

  it should "correctly leave fields empty when there is no content" in {
    val text1 =
      """
        |{{Infobox country
        ||conventional_long_name = Rome
        ||capital                =
        ||other                  = value
        |}}
        |""".stripMargin
    val text2 =
      """
        |{{Infobox country
        ||conventional_long_name = Rome
        ||capital                =
        |}}
        |""".stripMargin
    val testPage1 = sampleWikiPage(1, "Rome", text1)
    val testPage2 = sampleWikiPage(2, "Rome", text2)
    Country.fromInfobox(testPage1) should ===(Country(pageId = 1, pageTitle = "Rome", infoboxType = "country", conventionalName = "Rome").some)
    Country.fromInfobox(testPage2) should ===(Country(pageId = 2, pageTitle = "Rome", infoboxType = "country", conventionalName = "Rome").some)
  }

  it should "not extract fields without real content" in {
    val text =
      """
        |{{Infobox country
        ||conventional_long_name = Rome
        ||government_type        = <!--General information-->
        ||capital                = <!--not present-->
        ||religion               = <!--no data-->
        ||other                  = value
        |}}
        |""".stripMargin
    val testPage = sampleWikiPage(1, "Rome", text)
    Country.fromInfobox(testPage) should ===(Country(pageId = 1, pageTitle = "Rome", infoboxType = "country", conventionalName = "Rome").some)
  }
}
