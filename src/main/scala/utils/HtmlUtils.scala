package utils

import org.apache.commons.text.StringEscapeUtils

object HtmlUtils {
  /* converts HTML entities to utf-8 and weird characters with their common variant */
  def cleanHtmlString(s: String): String = {
    StringEscapeUtils.unescapeHtml4(s)
      .replaceAll("[\\u00A0\\u2007\\u202F]+", " ")
      .replaceAll("–", "-")
      .trim
  }
}
