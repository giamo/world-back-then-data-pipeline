package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import cats.implicits._
import com.github.giamo.wikihistory.models.wikipedia.Coordinates
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.giamo.wikihistory.UnitTestUtils.{readFromFile, sampleWikiPage}


final class SiteTest extends AnyFlatSpec with Matchers {

  "A site" should "be parsed from a 'settlement' infobox" in {
    val text = readFromFile("test_pages/settlement")
    val testPage = sampleWikiPage(1000, "The Hague (city)", text)

    Site.fromInfobox(testPage) should ===(
      Site(
        pageId = 1000,
        pageTitle = "The Hague (city)",
        name = "The Hague",
        coordinates = Some(Coordinates(52.083, 4.317))
      ).some
    )
  }

  it should "be parsed from a 'ancient site' infobox" in {
    val text = readFromFile("test_pages/ancient_site")
    val testPage = sampleWikiPage(2000, "Troy", text)

    Site.fromInfobox(testPage) should ===(
      Site(
        pageId = 2000,
        pageTitle = "Troy",
        name = "Troy",
        coordinates = Some(Coordinates(39.958, 26.239))
      ).some
    )
  }

  it should "extract the site name from a formatted string" in {
    val text =
      """{{Infobox ancient site
        || name = {{lower|0.1em|{{nobold|{{lang|gr| Athens }}}}}}
        |}}
        |""".stripMargin
    val testPage = sampleWikiPage(3000, "Athens", text)

    Site.fromInfobox(testPage) should ===(
      Site(pageId = 3000, pageTitle = "Athens", name = "Athens").some
    )
  }

  it should "be parsed from a 'greek dimos' infobox" in {
    val text = readFromFile("test_pages/greek_dimos")
    val testPage = sampleWikiPage(4000, "Kymi", text)


    Site.fromInfobox(testPage) should ===(
      Site(pageId = 4000, pageTitle = "Kymi", name = "Kymi", coordinates = Some(Coordinates(38.633, 24.1))).some
    )
  }

  it should "contain coordinates even if defined outside of the infobox" in {
    val text = readFromFile("test_pages/settlement_with_coordinates_outside")
    val testPage = sampleWikiPage(1000, "The Hague (city)", text)

    Site.fromInfobox(testPage) should ===(
      Site(1000, "The Hague (city)", "The Hague", Some(Coordinates(52.083, 4.317))).some
    )
  }

  it should "be parsed from the right infoboxes regardless of case-sensitivity)" in {
    val text1 = "{{Infobox Settlement\n| name = Rome\n|}}"
    val text2 = "{{Infobox Ancient Site\n| Name = Stonehenge\n|}}"
    val text3 = "{{Infobox Greek Dimos\n| name = Athens\n|}}"
    val testPage1 = sampleWikiPage(1, "Rome", text1)
    val testPage2 = sampleWikiPage(2, "Stonehenge", text2)
    val testPage3 = sampleWikiPage(3, "Athens", text3)

    Site.fromInfobox(testPage1) should ===(Site(pageId = 1, pageTitle = "Rome", name = "Rome").some)
    Site.fromInfobox(testPage2) should ===(Site(pageId = 2, pageTitle = "Stonehenge", name = "Stonehenge").some)
    Site.fromInfobox(testPage3) should ===(Site(pageId = 3, pageTitle = "Athens", name = "Athens").some)
  }
}
