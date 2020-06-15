package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import cats.implicits._
import com.github.giamo.wikihistory.UnitTestUtils.{readFromFile, _}
import com.github.giamo.wikihistory.models.wikipedia.Coordinates
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class MilitaryConflictTest extends AnyFlatSpec with Matchers {

  "A military conflict" should "be parsed from a 'military conflict' infobox" in {
    val text = readFromFile("test_pages/military_conflict")
    val testPage = sampleWikiPage(1, "Thirty Years War", text)

    MilitaryConflict.fromInfobox(testPage) should ===(
      MilitaryConflict(
        pageId = 1,
        pageTitle = "Thirty Years War",
        infoboxType = "military conflict",
        conflict = "Thirty Years' War",
        synopsis = "<p>The <b>Thirty Years' War</b> was a <a target=\"_blank\" href=\"https://en.wikipedia.org/wiki/war\" title=\"war\">war</a> fought primarily in <a target=\"_blank\" href=\"https://en.wikipedia.org/wiki/Central_Europe\" title=\"Central Europe\">Central Europe</a> between 1618 and 1648.</p>",
        partOf = "the [[European wars of religion]]".some,
        date = "23 May 1618 – 15 May 1648&lt;br /&gt;{{resize|''(29 years, 11 months, 3 weeks, and 1 day)''}}".some,
        place = "[[Central Europe]]".some,
        coordinates = Coordinates(32.7, -86.7).some
      ).some
    )
  }
}
