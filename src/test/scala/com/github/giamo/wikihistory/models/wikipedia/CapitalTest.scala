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
    fromString("[[Paris]]  (987–1682)") should ===(
      Capital(name = "[[Paris]]", dates = DateRange(Year(987), Year(1682)).some))
    fromString("[[Versailles (city)|Versailles]]  (1682–1789)") should ===(
      Capital(name = "[[Versailles (city)|Versailles]]", dates = DateRange(Year(1682), Year(1789)).some))

    // TODO: cover remaining corner cases
//    fromString("[[Paris]]  (987–1682;1789–1792; 1814–1848)") should ===()
//    fromString(
//      "{{ubl|[[Shechem]] <small>(930 BCE)</small>|[[Penuel]] <small>(930–909)</small>|[[Tirzah (ancient city)|Tirzah]] <small>(909–880)</small>}}"
//    ) should ===(Capital(name = ""))
//    fromString(
//      "[[Chang'an]]<small><br />(206 BC–9 AD, 190–195 AD)</small><br />[[Luoyang]]<small><br />(23–190 AD, 196 AD)</small><br />"
//    ) should ===(Capital(name = ""))
//    fromString(
//      "* [[Thebes, Egypt|Thebes]]<br>(1550 – c. 1352 BC, 17th dynasty and 18th dynasty before [[Akhenaten]])"
//    ) should ===(Capital(name = ""))
//    fromString(
//      "[[Ngàn Hống (capital)|Ngàn Hống]] (c. 2879 BC – ?)<br>[[Nghĩa Lĩnh (capital)|Nghĩa Lĩnh]] (29th c. BC)<br>[[Phong Châu]] (until 258&nbsp;BC)"
//    ) should ===(Capital(name = ""))
  }

  it should "remove references from the city's name" in {
    fromString(
      "[[Sriwijaya Kingdom Archaeological Park|Palembang]]&lt;ref name=&quot;Britannica-Srivijaya-Palembang&quot;&gt;{{Cite web|url=https://www.britannica.com}}&lt;/ref&gt;&lt;ref&gt;{{Cite news|url=https}}&lt;/ref&gt;"
    ) should ===(Capital("[[Sriwijaya Kingdom Archaeological Park|Palembang]]"))
  }
}
