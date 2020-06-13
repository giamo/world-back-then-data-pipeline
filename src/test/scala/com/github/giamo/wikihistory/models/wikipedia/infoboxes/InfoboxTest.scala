package com.github.giamo.wikihistory.models.wikipedia.infoboxes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.giamo.wikihistory.models.wikipedia.WikiPage

final class InfoboxTest extends AnyFlatSpec with Matchers {

  import Infobox._

  "Extracting a list from an infobox field" should "parse lists in wiki format" in {
    extractList(
      """
        |{{plainlist|
        |* [[Seleucia]]{{small|(305–240 BC)}}
        |* [[Antioch]]{{small|(240–63 BC)}}}}
        |""".stripMargin
    ) should ===(List(
      "[[Seleucia]]{{small|(305–240 BC)}}",
      "[[Antioch]]{{small|(240–63 BC)}}"
    )
    )
    extractList(
      """{{flatlist|
        |* [[Greek language|Greek]] {{small|(official)}}&lt;ref name=&quot;Frye&quot;&gt;Richard N. Frye
        |* [[Persian language|Persian]]
        |* [[Aramaic]]&lt;ref name=&quot;Frye&quot; /&gt;}}
        |""".stripMargin
    ) should ===(List(
      "[[Greek language|Greek]] {{small|(official)}}&lt;ref name=&quot;Frye&quot;&gt;Richard N. Frye",
      "[[Persian language|Persian]]",
      "[[Aramaic]]&lt;ref name=&quot;Frye&quot; /&gt;"
    )
    )
    extractList(
      """{{hlist|
        |* [[Avaldsnes|Ǫgvaldsnes]] (Avaldsnes)&lt;br /&gt;872–997
        |* [[Trondheim|Niðaróss]] (Trondheim)&lt;br /&gt;{{hlist|997–1016|1030–1111|1150–1217}}
        |* [[Sarpsborg|Borg]] (Sarpsborg)&lt;br /&gt;1016–1030
        |* [[Konghelle|Konungahella]] (Kungälv)&lt;br /&gt;1111 – {{circa|1150}}
        |* [[Bergen|Biorgvin]] (Bergen)&lt;br /&gt;1217–1314
        |* [[Oslo|Ósló]]&lt;br /&gt;1314–1397
        |}}
        |""".stripMargin
    ) should ===(List(
      "[[Avaldsnes|Ǫgvaldsnes]] (Avaldsnes)&lt;br /&gt;872–997",
      "[[Trondheim|Niðaróss]] (Trondheim)&lt;br /&gt;{{hlist|997–1016|1030–1111|1150–1217}}",
      "[[Sarpsborg|Borg]] (Sarpsborg)&lt;br /&gt;1016–1030",
      "[[Konghelle|Konungahella]] (Kungälv)&lt;br /&gt;1111 – {{circa|1150}}",
      "[[Bergen|Biorgvin]] (Bergen)&lt;br /&gt;1217–1314",
      "[[Oslo|Ósló]]&lt;br /&gt;1314–1397"
    )
    )
    extractList(
      """{{plainlist|
        |* [[Absolute monarchy]] and [[caliphate]] &lt;small&gt;(from 1517&lt;ref name=finkel111&gt;{{Cite book|last=Finkel |first=Caroline |title=Osman's Dream: The Story of the Ottoman Empire, 1300-1923 |place=New York |publisher=Basic Books |date=2005 |isbn=978-0-465-02396-7 |pages=110–1}}&lt;/ref&gt;)&lt;/small&gt;
        |* &lt;small&gt;(c. 1299–1876)&lt;/small&gt;
        |* &lt;small&gt;(1878–1908)&lt;/small&gt;
        |* &lt;small&gt;(1920–1922)&lt;/small&gt;
        |* [[Constitutional monarchy]]
        |* &lt;small&gt;(1876–1878)&lt;/small&gt;
        |* &lt;small&gt;(1908–1920)&lt;/small&gt;}}
        |""".stripMargin
      ) should ===(List(
        "[[Absolute monarchy]] and [[caliphate]] &lt;small&gt;(from 1517&lt;ref name=finkel111&gt;{{Cite book|last=Finkel |first=Caroline |title=Osman's Dream: The Story of the Ottoman Empire, 1300-1923 |place=New York |publisher=Basic Books |date=2005 |isbn=978-0-465-02396-7 |pages=110–1}}&lt;/ref&gt;)&lt;/small&gt;",
        "&lt;small&gt;(c. 1299–1876)&lt;/small&gt;",
        "&lt;small&gt;(1878–1908)&lt;/small&gt;",
        "&lt;small&gt;(1920–1922)&lt;/small&gt;",
        "[[Constitutional monarchy]]",
        "&lt;small&gt;(1876–1878)&lt;/small&gt;",
        "&lt;small&gt;(1908–1920)&lt;/small&gt;"
      )
    )
  }

  it should "work inside other wiki list tags" in {
    extractList(
      """{{Flatlist}}
        |* elem1
        |* elem2
        |{{Endflatlist}}
        |""".stripMargin
    ) should ===(List("elem1", "elem2"))
    extractList(
      """{{Plainlist}}
        |* [[Raqqada]] &lt;small&gt;(909–921)&lt;/small&gt;
        |* [[Mahdia]] &lt;small&gt;(921–948)&lt;/small&gt;
        |* [[Mansouria, Tunisia|al-Mansuriya]] &lt;small&gt;(948–973)&lt;/small&gt;
        |* [[Cairo]] &lt;small&gt;(973–1171)&lt;/small&gt;
        |{{Endplainlist}}
        |""".stripMargin
    ) should ===(List(
      "[[Raqqada]] &lt;small&gt;(909–921)&lt;/small&gt;",
      "[[Mahdia]] &lt;small&gt;(921–948)&lt;/small&gt;",
      "[[Mansouria, Tunisia|al-Mansuriya]] &lt;small&gt;(948–973)&lt;/small&gt;",
      "[[Cairo]] &lt;small&gt;(973–1171)&lt;/small&gt;"
    )
    )
  }

  it should "work with simple lists with asterisks" in {
    extractList(
      """* [[Sriwijaya Kingdom Archaeological Park|Palembang]]
        |* [[Kewu Plain]]
        |* [[Chaiya District]]
        |""".stripMargin
    ) should ===(List(
      "[[Sriwijaya Kingdom Archaeological Park|Palembang]]",
      "[[Kewu Plain]]",
      "[[Chaiya District]]"
    )
    )
  }

  it should "work with unbulleted lists" in {
    extractList(
      "{{unbulleted list|60.2% [[Church of Sweden]]{{efn|abc}} |8.5% [[Religion in Sweden|other religion]] |31.3% none/unspecified}}"
    ) should ===(List(
      "60.2% [[Church of Sweden]]{{efn|abc}}",
      "8.5% [[Religion in Sweden|other religion]]",
      "31.3% none/unspecified"
    )
    )
    extractList(
      "{{unbulleted list|60.2% [[Church of Sweden]] |8.5% [[Religion in Sweden|other religion]] |31.3% none/unspecified}}"
    ) should ===(List(
      "60.2% [[Church of Sweden]]",
      "8.5% [[Religion in Sweden|other religion]]",
      "31.3% none/unspecified"
    )
    )
  }

  it should "return the string as-is in case of a single element" in {
    extractList("[[Rome]]") should ===(List("[[Rome]]"))
    extractList("") shouldBe List("")
  }

}
