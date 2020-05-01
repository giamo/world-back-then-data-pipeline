package com.github.giamo.wikihistory.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class WikiCleanUtilsTest extends AnyFlatSpec with Matchers {
  import WikiCleanUtils._

  "The double braces remover" should "leave a string as-is when no full double braces are present" in {
    removeDoubleBraces("normal sentence!") should ===("normal sentence!")
    removeDoubleBraces("sentence with [[link]]") should ===("sentence with [[link]]")
    removeDoubleBraces("sentence with {single braces}.") should ===("sentence with {single braces}.")
    removeDoubleBraces("sentence with {{open braces not closed") should ===("sentence with {{open braces not closed")

  }

  it should "remove double braces, no matter how arbitrarily nested" in {
    removeDoubleBraces("{{this should be removed}}hello!") should ===("hello!")
    removeDoubleBraces("{{this {{should {{be}} removed}}}}hi!") should ===("hi!")
  }

  it should "not leave double spaces as a result of the removal" in {
    removeDoubleBraces("all {{this should be removed}} clean") should ===("all clean")
    removeDoubleBraces("{{this should be removed}} all trimmed") should ===("all trimmed")
  }
}
