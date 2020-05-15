package com.github.giamo.wikihistory.models.wikipedia

import cats.implicits._
import com.github.giamo.wikihistory.models.DateApproximation.GENERIC
import com.github.giamo.wikihistory.models.DatingLabel.BC
import com.github.giamo.wikihistory.models.{DateRange, Year}
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
        dates = DateRange(Year(305, BC), Year(240, BC)).some
      )
    )
    fromString("[[Zhaoge]] (ca. 1040 BC-661 BC)") should ===(
      Capital(
        name = "[[Zhaoge]]",
        dates = DateRange(Year(1040, BC, GENERIC), Year(661, BC)).some
      )
    )
    fromString("[[Assur]] 2025 BC") should ===(Capital(name = "[[Assur]]", dates = Year(2025, BC).some))
    fromString("[[Ngàn Hống (capital)|Ngàn Hống]] (c. 2879 BC)") should ===(
      Capital(name = "[[Ngàn Hống (capital)|Ngàn Hống]]", dates = Year(2879, BC, GENERIC).some))
    fromString("Paris (1200-1210)") should ===(
      Capital(name = "Paris", dates = DateRange(Year(1200), Year(1210)).some))
  }
}
