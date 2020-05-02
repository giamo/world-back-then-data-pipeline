package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.giamo.wikihistory.UnitTestUtils.{readFromFile, sampleWikiPage}


final class PhilosopherTest extends AnyFlatSpec with Matchers {

  "A philosopher" should "be parsed from a 'philosopher' infobox" in {
    val text = readFromFile("test_pages/philosopher")
    val testPage = sampleWikiPage(1, "Diogenes", text)

    Philosopher.fromInfobox(testPage) should ===(
      Philosopher(
        pageId = 1,
        pageTitle = "Diogenes",
        name = "Diogenes",
        birthDate = "{{circa}} 412 BC".some,
        birthPlace = "[[Sinop, Turkey|Sinope]]".some,
        schoolTraditions = "[[Greek philosophy]], [[Cynicism (philosophy)|Cynicism]]".some
      ).some
    )
  }
}
