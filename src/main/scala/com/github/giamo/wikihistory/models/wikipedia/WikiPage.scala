package com.github.giamo.wikihistory.models.wikipedia

import java.io.StringWriter
import java.net.URI

import com.github.giamo.wikihistory.utils.{HtmlUtils, WikiCleanUtils}
import net.java.textilej.parser.MarkupParser
import net.java.textilej.parser.builder.HtmlDocumentBuilder
import net.java.textilej.parser.markup.mediawiki.MediaWikiDialect
import org.apache.commons.lang3.StringUtils
import org.jsoup.parser.Parser

case class WikiPage(
  id: Long,
  title: String,
  text: String,
  isCategory: Boolean,
  isFile: Boolean,
  isTemplate: Boolean
)

object WikiPage {

  final val WikiBaseUri: URI = new URI("https://en.wikipedia.org")

  def getHtmlSynopsis(rawText: String): String = {
    val infoboxIndex = StringUtils.indexOfIgnoreCase(rawText, "{{infobox")
    val textWithoutHeading = if (infoboxIndex >=0) rawText.substring(infoboxIndex) else rawText

    val textUntilFirstParagraph = textWithoutHeading
      .split("\n")
      .takeWhile(l => !l.trim.startsWith("="))
      .mkString("\n")
    getCleanHtml(textUntilFirstParagraph)
  }

  def getCleanHtml(rawText: String, keepAllOneLine: Boolean = false): String = {
    val withoutDoubleBraces = WikiCleanUtils.removeDoubleBraces(rawText).trim
    val withoutReferences = WikiCleanUtils.removeReferences(withoutDoubleBraces)
    val withoutFileLinks = WikiCleanUtils.removeFileLinks(withoutReferences)
    val cleaned = WikiCleanUtils.cleanupLeftoverParenthesis(withoutFileLinks)
    convertToHtml(cleaned, keepAllOneLine)
  }

  private def convertToHtml(rawText: String, keepAllOneLine: Boolean): String = {
    val stringWriter = new StringWriter()
    val htmlBuilder = new HtmlDocumentBuilder(stringWriter)
    htmlBuilder.setBase(WikiBaseUri)
    htmlBuilder.setEmitAsDocument(false)

    val markupParser = new MarkupParser(new MediaWikiDialect)
    markupParser.setBuilder(htmlBuilder)
    markupParser.parse(rawText)

    val htmlText = stringWriter.toString
    val withFixedLinks = htmlText.replaceAll("<a ", """<a target="_blank" """)
    val withEscapedEntities = Parser.unescapeEntities(withFixedLinks, true)

    if (keepAllOneLine) withEscapedEntities
      .replaceAll("<p>", "")
      .replaceAll("(<br>|<br\\s*/\\s*>|</p>|\\s)+", " ")
      .trim
    else withEscapedEntities
  }

}