package com.github.giamo.wikihistory.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class HtmlUtilsTest extends AnyFlatSpec with Matchers {
  import HtmlUtils._

  "Cleaning an HTML string" should "not change it if nothing is to be replaced" in {
    cleanHtmlString("nothing special here") should ===("nothing special here")
  }

  it should "convert HTML entities to the corresponding utf-8 character" in {
    cleanHtmlString("this&nbsp;is&nbsp;an&nbsp;html&nbsp;string") should ===(
      "this is an html string"
    )
    cleanHtmlString("1&nbsp;is&nbsp;&lt;&nbsp;2") should ===("1 is < 2")
    cleanHtmlString("(1&lt;2&nbsp;&amp;&amp;&nbsp;x!=&quot;x&quot;)") should ===(
      """(1<2 && x!="x")"""
    )
  }

  it should "trim it correctly even with non-breaking spaces" in {
    cleanHtmlString("  &nbsp;&nbsp;hello&nbsp;world!&nbsp;    \t\n") should ===(
      "hello world!"
    )
  }
}
