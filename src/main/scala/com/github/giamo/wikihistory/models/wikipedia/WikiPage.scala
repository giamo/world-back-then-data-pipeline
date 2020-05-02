package com.github.giamo.wikihistory.models.wikipedia

import java.io.StringWriter

import com.github.giamo.wikihistory.utils.WikiCleanUtils
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
    val withoutDoubleBraces = WikiCleanUtils.removeDoubleBraces(textUntilFirstParagraph).trim
    val withoutReferences = WikiCleanUtils.removeReferences(withoutDoubleBraces)
    val cleaned = WikiCleanUtils.cleanupLeftoverParenthesis(withoutReferences)
    convertToHtml(cleaned)
  }

  private def convertToHtml(rawText: String): String = {
    // newlines are not correctly converted to html
    val preprocessedText = rawText.replaceAll("\n", "<br>")

    val stringWriter = new StringWriter()
    val htmlBuilder = new HtmlDocumentBuilder(stringWriter)
    htmlBuilder.setEmitAsDocument(false)

    val markupParser = new MarkupParser(new MediaWikiDialect)
    markupParser.setBuilder(htmlBuilder)
    markupParser.parse(preprocessedText)

    val htmlText = stringWriter.toString
    htmlText
  }

}