package com.github.giamo.wikihistory.models.wikipedia

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.giamo.wikihistory.UnitTestUtils.readFromFile

final class WikiPageTest extends AnyFlatSpec with Matchers {

  "A clean synopsis" should "not contain markup symbols" in {
    WikiPage.cleanSynopsis("This ''is'' a '''synopsis''' with a [[link]]") should ===(
      "This is a synopsis with a link"
    )
    WikiPage.cleanSynopsis("{{Infobox something}}Lorem ipsum") should ===(
      "Lorem ipsum"
    )
  }

  it should "contain only the first paragraph of text from a raw page" in {
    WikiPage.cleanSynopsis(
      """Lorem ipsum
       =Heading 1=
       blah
    """.trim
    ) should ===(
      "Lorem ipsum"
    )

    val rawText = readFromFile("test_pages/former_country")
    WikiPage.cleanSynopsis(rawText) should ===(
      """
        |The Empire of Japan was the historical nation-state and great power that existed from the Meiji Restoration in 1868 to the enactment of the 1947 constitution of modern Japan.
        |Japan's rapid industrialization and militarization led to its emergence as a world power.
        |""".stripMargin.trim
    )
  }
}
