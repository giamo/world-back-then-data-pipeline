package com.github.giamo.wikihistory.models.wikipedia

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class CapitalTest extends AnyFlatSpec with Matchers {
  import Capital._

  "Parsing a capital" should "extract the city's name" in {
    fromString("Rome") should ===(Capital("Rome"))
    fromString("[[Carthage]]") should ===(Capital("[[Carthage]]"))
  }

  it should "extract also the relevant dates, if present" in {
    fromString("[[Seleucia]]{{small|(305–240 BC)}}") should ===(
      Capital(
        name = "[[Seleucia]]",
        dates = "(305–240 BC)".some
      ))
  }
}
