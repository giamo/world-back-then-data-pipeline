package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.giamo.wikihistory.models.wikipedia.WikiPage

case class MockInfobox()
object MockInfobox extends Infobox[MockInfobox] {
  override val infoboxName = "mock"
  override def fromInfobox(page: WikiPage) = None
}

final class InfoboxTest extends AnyFlatSpec with Matchers {
  import MockInfobox._

  "Extracting a list from an infobox field" should "parse lists in wiki format" in {
    extractListFromRegex(cleanInfoboxText(
      text = """
        |{{Infobox mock
        ||capital = {{plainlist|
        |* [[Seleucia]]{{small|(305–240 BC)}}
        |* [[Antioch]]{{small|(240–63 BC)}}}}
        ||other = some
        |""".stripMargin),
      field = "capital"
    ) should ===(List(
      "[[Seleucia]]{{small|(305–240 BC)}}",
      "[[Antioch]]{{small|(240–63 BC)}}"
    ))

    extractListFromRegex(cleanInfoboxText(
      text = """
        |{{Infobox mock
        ||common_languages = {{plainlist|
        |* [[Greek language|Greek]] {{small|(official)}}&lt;ref name=&quot;Frye&quot;&gt;Richard N. Frye
        |* [[Persian language|Persian]]
        |* [[Aramaic]]&lt;ref name=&quot;Frye&quot; /&gt;}}
        ||other = some
        |""".stripMargin),
      field = "common_languages"
    ) should ===(List(
      "[[Greek language|Greek]] {{small|(official)}}&lt;ref name=&quot;Frye&quot;&gt;Richard N. Frye",
      "[[Persian language|Persian]]",
      "[[Aramaic]]&lt;ref name=&quot;Frye&quot; /&gt;"
    ))
  }

  it should "return the string as-is in case of a single element" in {
    extractListFromRegex(
      text = cleanInfoboxText("{{Infobox mock\n|capital = [[Rome]]\n|other = some}}"),
      field = "capital"
    ) should ===(List("[[Rome]]"))
    extractListFromRegex(
      text = cleanInfoboxText("{{Infobox mock\n|capital = \n|other = some}}"),
      field = "capital"
    ) shouldBe empty
  }
}
