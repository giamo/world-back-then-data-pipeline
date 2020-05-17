package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.giamo.wikihistory.models.wikipedia.WikiPage

final class InfoboxTest extends AnyFlatSpec with Matchers {
  import Infobox._

  "Extracting a list from an infobox field" should "parse lists in wiki format" in {
    extractList("""
        |{{plainlist|
        |* [[Seleucia]]{{small|(305–240 BC)}}
        |* [[Antioch]]{{small|(240–63 BC)}}}}
        |""".stripMargin
    ) should ===(List(
      "[[Seleucia]]{{small|(305–240 BC)}}",
      "[[Antioch]]{{small|(240–63 BC)}}"
    ))

    extractList("""{{plainlist|
        |* [[Greek language|Greek]] {{small|(official)}}&lt;ref name=&quot;Frye&quot;&gt;Richard N. Frye
        |* [[Persian language|Persian]]
        |* [[Aramaic]]&lt;ref name=&quot;Frye&quot; /&gt;}}
        |""".stripMargin
    ) should ===(List(
      "[[Greek language|Greek]] {{small|(official)}}&lt;ref name=&quot;Frye&quot;&gt;Richard N. Frye",
      "[[Persian language|Persian]]",
      "[[Aramaic]]&lt;ref name=&quot;Frye&quot; /&gt;"
    ))
  }

  it should "return the string as-is in case of a single element" in {
    extractList("[[Rome]]") should ===(List("[[Rome]]"))
    extractList("") shouldBe List("")
  }
}
