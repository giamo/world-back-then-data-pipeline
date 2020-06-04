package com.github.giamo.wikihistory.utils

import org.apache.commons.lang3.StringUtils

import scala.annotation.tailrec

object WikiCleanUtils {
  private final val NoBraces = 0
  private final val OpenBrace = 1
  private final val CloseBrace = 2

  // remove all all double braces and their content (single or arbitrarily nested)
  def removeDoubleBraces(
    text: String,
    braceSymbolOpen: Char = '{',
    braceSymbolClose: Char = '}',
    bracesPrefix: String = ""
  ): String = {
    @tailrec
    def removeFirstDoubleBracesRec(s: String, i: Int, cur: Int, state: Int, level: Int): String = {
      if (cur == s.length) s.substring(0, i)
      else {
        val currentChar = s.charAt(cur)
        val newLevel = (state, currentChar) match {
          case (OpenBrace, `braceSymbolOpen`) => level + 1
          case (CloseBrace, `braceSymbolClose`) => level - 1
          case _ => level
        }

        val newState = (state, currentChar) match {
          case (_, `braceSymbolOpen`) => OpenBrace
          case (OpenBrace, _) => NoBraces
          case (CloseBrace, `braceSymbolClose`) => NoBraces
          case (_, `braceSymbolClose`) => CloseBrace
          case _ => state
        }

        if (newLevel == 0 && cur == s.length) s.substring(0, i)
        else if (newLevel == 0) s.substring(0, i) + s.substring(cur + 1, s.length)
        else removeFirstDoubleBracesRec(s, i, cur + 1, newState, newLevel)
      }
    }

    @tailrec
   def removeAllDoubleBracesRec(remainingText: String): String = {
      val i1 = StringUtils.indexOfIgnoreCase(remainingText, s"${braceSymbolOpen}${braceSymbolOpen}$bracesPrefix")
      val i2 = StringUtils.indexOfIgnoreCase(remainingText, s"${braceSymbolClose}${braceSymbolClose}")

      if (i1 == -1 || i2 == -1) remainingText
      else {
        val initialState = NoBraces
        val initialLevel = 1
        val remaining = removeFirstDoubleBracesRec(remainingText, i1, i1 + 2, initialState, initialLevel)
        removeAllDoubleBracesRec(remaining)
      }
    }

    removeAllDoubleBracesRec(text)
      .replaceAll("[ ]+", " ")
      .trim
  }

  def removeReferences(s: String): String =
    s.replaceAll("(?s)(?:<ref|&lt;ref).*?(?:/ref>|/>|/ref&gt;|/&gt;)", "")

  def removeFileLinks(s: String): String =
      removeDoubleBraces(s, braceSymbolOpen = '[', braceSymbolClose = ']', bracesPrefix = "file:")

  def removeHtmlComments(s: String): String = s.replaceAll("<!--.*?-->", "");

  // empty or dirty parenthesis can be left over after removing references
  def cleanupLeftoverParenthesis(s: String): String = s
    // remove empty parenthesis
    .replaceAll("\\s?\\([^a-zA-Z0-9]*\\)", "")
    // remove parenthesis starting or ending with leftover isolated punctuation or spaces
    .replaceAll("(\\s?\\([,.;:\\s].*?\\))|(\\s?\\(.*?[,.;:\\s]\\))", "")

}
