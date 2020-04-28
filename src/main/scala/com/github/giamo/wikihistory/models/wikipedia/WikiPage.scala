package com.github.giamo.wikihistory.models.wikipedia

import org.wikiclean.WikiClean
import org.wikiclean.languages.English

case class WikiPage(
  id: Long,
  title: String,
  text: String,
  isCategory: Boolean,
  isFile: Boolean,
  isTemplate: Boolean
)

object WikiPage {
  private val wikiCleaner = new WikiClean.Builder().withLanguage(new English).build

  // TODO: treat special cases of {{formatted strings}}
  def cleanSynopsis(rawText: String): String = {
    val textUntilFirstParagraph = rawText
      .split("\n")
      .takeWhile(l => !l.trim.startsWith("="))
      .mkString("\n")
    wikiCleaner.clean("<text xml:space=\"preserve\">" + textUntilFirstParagraph + "</text>").trim
  }

//  def htmlContent(rawText: String) = {
    //    val wikiPatternMatcher = new WikiPatternMatcher(rawText)
    //    val text = wikiPatternMatcher.getText
    //    val infoboxText = wikiPatternMatcher.getInfoBox().dumpRaw()
    //    val cleanText = cleanInfoboxText(infoboxText)
    //    val x2 = text.substring(infoboxText.length)


    //    import net.java.textilej.parser.MarkupParser
    //    import net.java.textilej.parser.builder.HtmlDocumentBuilder
    //    import net.java.textilej.parser.markup.mediawiki.MediaWikiDialect
    //    val writer = new StringWriter()
    //    import net.java.textilej.parser.MarkupParser
    //    import net.java.textilej.parser.builder.HtmlDocumentBuilder
    //    import net.java.textilej.parser.markup.mediawiki.MediaWikiDialect
    //
    //    val builder = new HtmlDocumentBuilder(writer)
    //    builder.setEmitAsDocument(false)
    //
    //    val parser = new MarkupParser(new MediaWikiDialect)
    //    parser.setBuilder(builder)
    //    parser.parse(rawText)
    //
    //    val html = writer.toString
//  }
}