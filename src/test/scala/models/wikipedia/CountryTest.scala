package models.wikipedia

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class CountryTest extends AnyFlatSpec with Matchers {

  "A country" should "be parsed from a 'country' infobox" in {
    val text =
      """{{Infobox country
        |&lt;!-- See Template:Infobox settlement for additional fields and descriptions --&gt;
        || common_name  = Sparta
        || conventional_long_name = Spartan Kingdom
        || other_name = Sparty
        ||year_start=500BC
        |}}
        |""".stripMargin

    Country.fromInfobox(text, 1000) should ===(
      Country(
        conventionalName = "Spartan Kingdom",
        name = Some("Sparta"),
        yearStart = Some("500BC"),
        fromPage = 1000
      ).some
    )
  }

  it should "be parsed from a 'former country' infobox" in {
    val text =
      """{{Infobox former country
        ||conventional_long_name=Roman Republic
        || perdiod =    ancient
        ||year_end      = 27BC
        |}}
        |""".stripMargin

    Country.fromInfobox(text, 2000) should ===(
      Country(
        conventionalName = "Roman Republic",
        yearEnd = Some("27BC"),
        fromPage = 2000
      ).some
    )
  }

  it should "extract country names from formatted strings" in {
    val text =
      """{{Infobox former country
        || conventional_long_name = {{raise|0.2em|Xianyang}}
        |}}
        |""".stripMargin

    Country.fromInfobox(text, 2000) should ===(
      Country(conventionalName = "Xianyang", fromPage = 2000).some
    )
  }
}
