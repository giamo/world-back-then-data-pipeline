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

  it should "preserve the original newlines as html <br>'s, if specified" in {
    WikiPage.getCleanHtml(
      rawText = """
        |Mauretania is an ancient region.
        |
        |In 27 BC, the kings of Mauretania became Roman vassals.
        |""".stripMargin,
      keepLineBreaks = false
    ) should ===(
      "<p>Mauretania is an ancient region.</p><p>In 27 BC, the kings of Mauretania became Roman vassals.</p>"
    )
  }

  it should "not preserve the original newlines, if specified" in {
    WikiPage.getCleanHtml(
      """
        |'''Mauretania''' is a region in the ancient [[Maghreb]].
        |
        |In 27 BC, the kings of Mauretania became Roman vassals.
        |""".stripMargin
    ) should ===(
      """<p><b>Mauretania</b> is a region in the ancient <a target="_blank" href="https://en.wikipedia.org/wiki/Maghreb" title="Maghreb">Maghreb</a>.""" +
        "<br/><br/>In 27 BC, the kings of Mauretania became Roman vassals.</p>"
    )
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
      """<p>The <b>Empire of Japan</b> was the historical <a target="_blank" href="https://en.wikipedia.org/wiki/nation-state" title="nation-state">nation-state</a> and <a target="_blank" href="https://en.wikipedia.org/wiki/great_power" title="great power">great power</a> that existed from the <a target="_blank" href="https://en.wikipedia.org/wiki/Meiji_Restoration" title="Meiji Restoration">Meiji Restoration</a> in 1868 to the enactment of the <a target="_blank" href="https://en.wikipedia.org/wiki/Constitution_of_Japan" title="Constitution of Japan">1947 constitution</a> of modern <a target="_blank" href="https://en.wikipedia.org/wiki/Japan" title="Japan">Japan</a>.<br/>""" +
        """Japan's rapid <a target="_blank" href="https://en.wikipedia.org/wiki/industrialization" title="industrialization">industrialization</a> and <a target="_blank" href="https://en.wikipedia.org/wiki/militarization" title="militarization">militarization</a> led to its emergence as a <a target="_blank" href="https://en.wikipedia.org/wiki/world_power" title="world power">world power</a>.</p>"""
    )
  }
}
