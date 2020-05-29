package com.github.giamo.wikihistory.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class WikiCleanUtilsTest extends AnyFlatSpec with Matchers {

  import WikiCleanUtils._

  "Removing double braces" should "leave a string as-is when no full double braces are present" in {
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

  "Removing references" should "drop all text inside <ref> tags" in {
    removeReferences("No references<ref x='y'> some reference</ref> here<ref inline/>.") should ===(
      "No references here."
    )
    removeReferences("No references<ref x='y'> some reference\n with newline</ref> here<ref inline/>.") should ===(
      "No references here."
    )
    removeReferences("No references&lt;ref&gt; some reference&lt;/ref&gt; here&lt;ref inline/&gt;.") should ===(
      "No references here."
    )
  }

  "Cleaning leftover parenthesis" should "drop parenthesis without any alphanumerical characters" in {
    cleanupLeftoverParenthesis("cleaned () up text") should ===("cleaned up text")
    cleanupLeftoverParenthesis("cleaned up ( ,) text") should ===("cleaned up text")
    cleanupLeftoverParenthesis("cleaned up text(:: ;)") should ===("cleaned up text")
  }

  it should "drop parenthesis with punctuation or a space as the first or last character" in {
    cleanupLeftoverParenthesis("cleaned up (, with cleaned parenthesis) text") should ===(
      "cleaned up text")
    cleanupLeftoverParenthesis("cleaned up (with cleaned parenthesis, ) text") should ===(
      "cleaned up text")
    cleanupLeftoverParenthesis("Corinth (; Kórinthos; Doric Greek: Kórinthos)") should ===("Corinth")
    cleanupLeftoverParenthesis("The Odrysian Kingdom (; Ancient Greek: ; ) was") should ===("The Odrysian Kingdom was")
    cleanupLeftoverParenthesis("AAA (: BBB)") should ===("AAA")
    cleanupLeftoverParenthesis("Atropatene (in Old Iranian: Ātṛpātakāna; in Greek: )") should ===("Atropatene")
    cleanupLeftoverParenthesis("The Parthian Empire ( 247 BC – 224 AD)") should ===("The Parthian Empire")
    cleanupLeftoverParenthesis("Corinth or Korinthos ( or )") should ===("Corinth or Korinthos")
  }

  "Removing file links" should "replace double square braces starting with 'file:'" in {
    removeFileLinks("[[File:Edom.PNG|right|thumb|upright|Map]] text without link") should ===(
      "text without link"
    )
    removeFileLinks("[[file:Rome.gif|caption]] text without link") should ===(
      "text without link"
    )
  }

  it should "also work correctly when there are nested links inside file links" in {
    removeFileLinks("[[File:Edom.PNG|right|thumb|[[link text|link url]]|Map]] text without link") should ===(
      "text without link"
    )
    removeFileLinks("[[file:Rome.gif|caption]] text without file link but with [[normal link]]") should ===(
      "text without file link but with [[normal link]]"
    )
  }

}
