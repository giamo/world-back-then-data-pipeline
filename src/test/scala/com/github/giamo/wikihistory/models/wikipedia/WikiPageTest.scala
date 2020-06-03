package com.github.giamo.wikihistory.models.wikipedia

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.giamo.wikihistory.UnitTestUtils.readFromFile

final class WikiPageTest extends AnyFlatSpec with Matchers {

  "A clean html converted from markup" should "have correct style and links" in {
    WikiPage.getCleanHtml("This ''is'' a '''synopsis'''") should ===(
      "<p>This <i>is</i> a <b>synopsis</b></p>"
    )
  }

  it should "have links prefixed with wikipedia's base url" in {
    WikiPage.getCleanHtml("Text with a [[link|link text]]") should ===(
      "<p>Text with a <a target=\"_blank\" href=\"https://en.wikipedia.org/wiki/link\" title=\"link\">link text</a></p>"
    )
  }

  it should "preserve the original newlines as html <p>'s" in {
    WikiPage.getCleanHtml(
      rawText = """
        |Mauretania is an ancient region.
        |
        |In 27 BC, the kings of Mauretania became Roman vassals.
        |""".stripMargin
    ) should ===(
      "<p>Mauretania is an ancient region.</p><p>In 27 BC, the kings of Mauretania became Roman vassals.</p>"
    )
  }

  it should "keep all text on one line, if specified" in {
    WikiPage.getCleanHtml(
      rawText = """
        |Mauretania is an ancient region. <br><br/><br />
        |
        |In 27 BC, the kings of Mauretania became Roman vassals.
        |""".stripMargin,
      keepAllOneLine = true
    ) should ===("Mauretania is an ancient region. In 27 BC, the kings of Mauretania became Roman vassals.")
  }

  it should "not contain references or text inside double braces" in {
    WikiPage.getCleanHtml("No references<ref x='y'> some reference</ref> here<ref inline/>.") should ===(
      "<p>No references here.</p>")
    WikiPage.getCleanHtml("{{Infobox something}}Lorem ipsum {{footer}}") should ===(
      "<p>Lorem ipsum</p>"
    )
  }

  it should "not clean leftover parenthesis" in {
    WikiPage.getCleanHtml(
      """The region of Mauretania ({{IPAc-en|ˌ|m|ɒr|ɪ|ˈ|t|eɪ|n|i|ə|,_|ˌ|m|ɔːr|ɪ|-}}) () (Maghreb)."""
    ) should ===("""<p>The region of Mauretania (Maghreb).</p>""")
  }

  "A clean synopsis" should "contain only the first paragraph of text from a raw page" in {
    WikiPage.getHtmlSynopsis(
      """Lorem ipsum
       =Heading 1=
       blah
    """.trim
    ) should ===(
      "<p>Lorem ipsum</p>"
    )

    val rawText = readFromFile("test_pages/former_country")
    WikiPage.getHtmlSynopsis(rawText) should ===(
      """<p>The <b>Empire of Japan</b> was the historical <a target="_blank" href="https://en.wikipedia.org/wiki/nation-state" title="nation-state">nation-state</a> and <a target="_blank" href="https://en.wikipedia.org/wiki/great_power" title="great power">great power</a> that existed from the <a target="_blank" href="https://en.wikipedia.org/wiki/Meiji_Restoration" title="Meiji Restoration">Meiji Restoration</a> in 1868 to the enactment of the <a target="_blank" href="https://en.wikipedia.org/wiki/Constitution_of_Japan" title="Constitution of Japan">1947 constitution</a> of modern <a target="_blank" href="https://en.wikipedia.org/wiki/Japan" title="Japan">Japan</a>.</p>""" +
        """<p>Japan's rapid <a target="_blank" href="https://en.wikipedia.org/wiki/industrialization" title="industrialization">industrialization</a> and <a target="_blank" href="https://en.wikipedia.org/wiki/militarization" title="militarization">militarization</a> led to its emergence as a <a target="_blank" href="https://en.wikipedia.org/wiki/world_power" title="world power">world power</a>.</p>"""
    )
  }

  it should "not include any text appearing before infoboxes" in {
    val rawText =
      """
        |:''For the English football league, see [[Aetolian League (football)]].''
        |
        |{{Infobox country
        ||native_name =''Koinon tōn Aitōlōn''
        |}}
        |The Aetolian League was a confederation of tribal communities and cities""".stripMargin

    WikiPage.getHtmlSynopsis(rawText) should ===("<p>The Aetolian League was a confederation of tribal communities and cities</p>")
  }
}
