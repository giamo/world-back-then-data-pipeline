package com.github.giamo.wikihistory.utils

import org.apache.commons.lang.CharEncoding
import org.apache.commons.lang3.StringUtils
import org.jsoup.Jsoup
import org.jsoup.nodes.Entities.EscapeMode
import org.jsoup.parser.Parser

object HtmlUtils {

  /* removes html tags and converts HTML entities to utf-8 and weird characters with their common variant */
  def cleanHtmlString(s: String): String = {
    val parsedDocument = Jsoup.parse(s, StringUtils.EMPTY, Parser.htmlParser)
    parsedDocument.outputSettings.escapeMode(EscapeMode.base)
    parsedDocument.outputSettings.charset(CharEncoding.UTF_8)
    parsedDocument.select("br").append("\\n")

    Parser.unescapeEntities(parsedDocument.text, true)
      .replaceAll("\\\\n", "\n")
      .replaceAll("[\\u00A0\\u2007\\u202F]+", " ")
      .replaceAll("–", "-")
      .trim
  }
}
