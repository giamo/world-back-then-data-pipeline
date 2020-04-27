package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class PhilosopherTest extends AnyFlatSpec with Matchers {

  "A philosopher" should "be parsed from a 'philosopher' infobox" in {
    val text =
      """{{Infobox philosopher
        || region           = [[Western philosophy]]
        || era              = [[Ancient philosophy]]
        || image            = Waterhouse-Diogenes.jpg
        || caption          = ''Diogenes'' (1882) &lt;br /&gt;by [[John William Waterhouse]]
        ||name              = Diogenes
        || birth_date       = {{circa}} 412 BC
        || birth_place      = [[Sinop, Turkey|Sinope]]
        || death_date       = 323 BC (aged about 89)
        || death_place      = [[Corinth]]
        || school_tradition = [[Greek philosophy]], [[Cynicism (philosophy)|Cynicism]]
        || main_interests   = [[Asceticism]], [[Cynicism (philosophy)|Cynicism]]
        || notable_ideas    = {{plainlist|
        |*[[Cynicism (philosophy)|Cynic philosophy]]
        |*[[Cosmopolitanism]]
        |*''[[Solvitur ambulando]]''}}
        || influences       = [[Antisthenes]], [[Socrates]]
        || influenced       = [[Crates of Thebes]], other [[Cynicism (philosophy)|Cynics]], [[Epicurus]], the [[Stoics]], Wolfi Landstreicher, [[Han Ryner]], [[Michel Onfray]], [[Søren Kierkegaard]]
        |}}
        |""".stripMargin

    Philosopher.fromInfobox(text, 1) should ===(
      Philosopher(
        "Diogenes",
        "{{circa}} 412 BC".some,
        "[[Sinop, Turkey|Sinope]]".some,
        "[[Greek philosophy]], [[Cynicism (philosophy)|Cynicism]]".some,
        1
      ).some
    )
  }
}
