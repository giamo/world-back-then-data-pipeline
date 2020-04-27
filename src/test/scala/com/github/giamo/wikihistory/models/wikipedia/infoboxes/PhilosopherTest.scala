package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.github.giamo.wikihistory.UnitTestUtils.readFromFile


final class PhilosopherTest extends AnyFlatSpec with Matchers {

  "A philosopher" should "be parsed from a 'philosopher' infobox" in {
    val text = readFromFile("test_pages/philosopher")

    Philosopher.fromInfobox(text, 1) should ===(
      Philosopher(
        "Diogenes",
        "{{circa}} 412 BC".some,
        "[[Sinop, Turkey|Sinope]]".some,
        "[[Greek philosophy]], [[Cynicism (philosophy)|Cynicism]]".some,
        1
      ).some
    )
  }
}
