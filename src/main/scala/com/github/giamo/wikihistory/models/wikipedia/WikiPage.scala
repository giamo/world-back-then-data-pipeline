package com.github.giamo.wikihistory.models.wikipedia

import java.io.StringWriter

import com.github.giamo.wikihistory.utils.{HtmlUtils, WikiCleanUtils}
import net.java.textilej.parser.MarkupParser
import net.java.textilej.parser.builder.HtmlDocumentBuilder
import net.java.textilej.parser.markup.mediawiki.MediaWikiDialect

case class WikiPage(
  id: Long,
  title: String,
  text: String,
  isCategory: Boolean,
  isFile: Boolean,
  isTemplate: Boolean
)

object WikiPage {

  def getHtmlSynopsis(rawText: String): String = {
    val textUntilFirstParagraph = rawText
      .split("\n")
      .takeWhile(l => !l.trim.startsWith("="))
      .mkString("\n")
    getCleanHtml(textUntilFirstParagraph)
  }

  def getCleanHtml(rawText: String, keepLineBreaks: Boolean = true): String = {
    val withoutDoubleBraces = WikiCleanUtils.removeDoubleBraces(rawText).trim
    val withoutReferences = WikiCleanUtils.removeReferences(withoutDoubleBraces)
    val withoutFileLinks = WikiCleanUtils.removeFileLinks(withoutReferences)
    val cleaned = WikiCleanUtils.cleanupLeftoverParenthesis(withoutFileLinks)
    convertToHtml(cleaned, keepLineBreaks)
  }

  private def convertToHtml(rawText: String, keepLineBreaks: Boolean): String = {
    // newlines are not correctly converted to html
    val preprocessedText =
      if (keepLineBreaks) rawText.replaceAll("\n", "<br>")
      else rawText

    val stringWriter = new StringWriter()
    val htmlBuilder = new HtmlDocumentBuilder(stringWriter)
    htmlBuilder.setEmitAsDocument(false)

    val markupParser = new MarkupParser(new MediaWikiDialect)
    markupParser.setBuilder(htmlBuilder)
    markupParser.parse(preprocessedText)

    val htmlText = stringWriter.toString
    htmlText.replaceAll("""href="/wiki/""", """target="_blank" href="https://en.wikipedia.org/wiki/""")
  }

}