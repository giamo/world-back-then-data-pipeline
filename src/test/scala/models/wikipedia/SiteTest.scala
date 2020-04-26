package models.wikipedia

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SiteTest extends AnyFlatSpec with Matchers {

  "A site" should "be parsed from a 'settlement' infobox" in {
    val text =
      """{{Infobox settlement
        |&lt;!-- See Template:Infobox settlement for additional fields and descriptions --&gt;
        || name                    = The Hague
        || native_name             = {{native name|nl|Den Haag|icon=no}}
        || coordinates = {{coord|52|5|N|4|19|E|region:NL|display=inline,title}}
        || map_caption             = Location in South Holland
        |}}
        |""".stripMargin

    Site.fromInfobox(text, 1000) should ===(
      Site("The Hague", Some(Coordinates(52.083, 4.317)), 1000).some
    )
  }

  it should "be parsed from a 'ancient site' infobox" in {
    val text =
      """{{Infobox ancient site
        || name = Stonehenge
        |}}
        |""".stripMargin

    Site.fromInfobox(text, 2000) should ===(
      Site(name = "Stonehenge", fromPage = 2000).some
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
    val text =
      """{{Infobox greek dimos
        || name = Kymi, Greece
        |}}
        |""".stripMargin

    Site.fromInfobox(text, 3000) should ===(
      Site(name = "Kymi, Greece", fromPage = 3000).some
    )
  }
}
