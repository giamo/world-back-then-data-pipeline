package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import cats.implicits._
import com.github.giamo.wikihistory.models.wikipedia.Coordinates
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.github.giamo.wikihistory.UnitTestUtils.readFromFile


final class SiteTest extends AnyFlatSpec with Matchers {

  "A site" should "be parsed from a 'settlement' infobox" in {
    val text = readFromFile("test_pages/settlement")

    Site.fromInfobox(text, 1000) should ===(
      Site("The Hague", Some(Coordinates(52.083, 4.317)), 1000).some
    )
  }

  it should "be parsed from a 'ancient site' infobox" in {
    val text = readFromFile("test_pages/ancient_site")

    Site.fromInfobox(text, 2000) should ===(
      Site(name = "Troy", coordinates = Some(Coordinates(39.958, 26.239)), fromPage = 2000).some
    )
  }

  it should "extract the site name from a formatted string" in {
    val text =
      """{{Infobox ancient site
        || name = {{lower|0.1em|{{nobold|{{lang|gr| Athens }}}}}}
        |}}
        |""".stripMargin

    Site.fromInfobox(text, 2000) should ===(
      Site(name = "Athens", fromPage = 2000).some
    )
  }

  it should "be parsed from a 'greek dimos' infobox" in {
    val text = readFromFile("test_pages/greek_dimos")

    Site.fromInfobox(text, 3000) should ===(
      Site(name = "Kymi", coordinates = Some(Coordinates(38.633, 24.1)), fromPage = 3000).some
    )
  }

  it should "contain coordinates even if defined outside of the infobox" in {
    val text = readFromFile("test_pages/settlement_with_coordinates_outside")

    Site.fromInfobox(text, 1000) should ===(
      Site("The Hague", Some(Coordinates(52.083, 4.317)), 1000).some
    )
  }
}
