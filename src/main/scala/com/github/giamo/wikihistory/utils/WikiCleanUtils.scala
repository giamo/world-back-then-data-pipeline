package com.github.giamo.wikihistory.utils

import scala.annotation.tailrec

object WikiCleanUtils {
  private final val NoBraces = 0
  private final val OpenBrace = 1
  private final val CloseBrace = 2

  // remove all all double braces and their content (single or arbitrarily nested)
  def removeDoubleBraces(text: String): String = {
    @tailrec
    def removeFirstDoubleBracesRec(s: String, i: Int, cur: Int, state: Int, level: Int): String = {
      if (cur == s.length) s.substring(0, i)
      else {
        val currentChar = s.charAt(cur)
        val newLevel = (state, currentChar) match {
          case (OpenBrace, '{') => level + 1
          case (CloseBrace, '}') => level - 1
          case _ => level
        }

        val newState = (state, currentChar) match {
          case (_, '{') => OpenBrace
          case (OpenBrace, _) => NoBraces
          case (CloseBrace, '}') => NoBraces
          case (_, '}') => CloseBrace
          case _ => state
        }

        if (newLevel == 0 && cur == s.length) s.substring(0, i)
        else if (newLevel == 0) s.substring(0, i) + s.substring(cur + 1, s.length)
        else removeFirstDoubleBracesRec(s, i, cur + 1, newState, newLevel)
      }
    }

    @tailrec
    def removeAllDoubleBracesRec(remainingText: String): String = {
      val i1 = remainingText.indexOf("{{")
      val i2 = remainingText.indexOf("}}")

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
}
