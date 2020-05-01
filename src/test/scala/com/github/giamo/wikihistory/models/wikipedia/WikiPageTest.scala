package com.github.giamo.wikihistory.models.wikipedia

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.giamo.wikihistory.UnitTestUtils.readFromFile

final class WikiPageTest extends AnyFlatSpec with Matchers {

  "A clean synopsis" should "be converted from markup to html" in {
    WikiPage.getHtmlSynopsis("This ''is'' a '''synopsis''' with a [[link]]") should ===(
      "<p>This <i>is</i> a <b>synopsis</b> with a <a href=\"/wiki/link\" title=\"link\">link</a></p>"
    )
    WikiPage.getHtmlSynopsis("{{Infobox something}}Lorem ipsum {{footer}}") should ===(
      "<p>Lorem ipsum</p>"
    )
  }

  it should "contain only the first paragraph of text from a raw page" in {
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
      """<p>The <b>Empire of Japan</b> was the historical <a href="/wiki/nation-state" title="nation-state">nation-state</a> and <a href="/wiki/great_power" title="great power">great power</a> that existed from the <a href="/wiki/Meiji_Restoration" title="Meiji Restoration">Meiji Restoration</a> in 1868 to the enactment of the <a href="/wiki/Constitution_of_Japan" title="Constitution of Japan">1947 constitution</a> of modern <a href="/wiki/Japan" title="Japan">Japan</a>.<br/>""" +
        """Japan's rapid <a href="/wiki/industrialization" title="industrialization">industrialization</a> and <a href="/wiki/militarization" title="militarization">militarization</a> led to its emergence as a <a href="/wiki/world_power" title="world power">world power</a>.</p>"""
    )
  }

  it should "preserve the original newlines as html <br>'s" in {
    WikiPage.getHtmlSynopsis(
      """
        |'''Mauretania''' is a region in the ancient [[Maghreb]].
        |
        |In 27 BC, the kings of Mauretania became Roman vassals.
        |""".stripMargin
    ) should ===(
      """<p><b>Mauretania</b> is a region in the ancient <a href="/wiki/Maghreb" title="Maghreb">Maghreb</a>.""" +
        "<br/><br/>In 27 BC, the kings of Mauretania became Roman vassals.</p>"
    )
  }

  it should "not contain references" in {
    WikiPage.getHtmlSynopsis("No references<ref x='y'> some reference</ref> here<ref inline/>.") should ===(
      "<p>No references here.</p>")
    WikiPage.getHtmlSynopsis("No references<ref x='y'> some reference\n with newline</ref> here<ref inline/>.") should ===(
      "<p>No references here.</p>")
    WikiPage.getHtmlSynopsis("No references&lt;ref&gt; some reference&lt;/ref&gt; here&lt;ref inline/&gt;.") should ===(
      "<p>No references here.</p>")
  }

  it should "not contain leftover parenthesis without any alphanumerical characters" in {
    WikiPage.getHtmlSynopsis(
      """The region of Mauretania ({{IPAc-en|ˌ|m|ɒr|ɪ|ˈ|t|eɪ|n|i|ə|,_|ˌ|m|ɔːr|ɪ|-}}) (Maghreb)."""
    ) should ===("""<p>The region of Mauretania (Maghreb).</p>""")
    WikiPage.getHtmlSynopsis(
      """The region of Mauretania ({{IPAc-en|ˌ|m|ɒr|ɪ|ˈ|t|eɪ|n|i|ə|,_|ˌ|m|ɔːr|ɪ|-}}, {{language:arab}}) (Maghreb)."""
    ) should ===("""<p>The region of Mauretania (Maghreb).</p>""")
  }
}
