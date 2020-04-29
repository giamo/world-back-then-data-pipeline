package com.github.giamo.wikihistory.models.wikipedia

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.giamo.wikihistory.UnitTestUtils.readFromFile

final class CoordinatesTest extends AnyFlatSpec with Matchers {

  import Coordinates._

  "Parsing a coordinates string" should "extract and compute the right lat and long" in {
    fromTemplate("{{coord|40|25|4|N|85|38|56|W|region:US-IN|display=inline,title}}") should ===(
      Coordinates(40.418, -85.649).some
    )
    fromTemplate("{{coord|8|50|18|S|13|14|4|E|region:AO|display=inline}}") should ===(
      Coordinates(-8.838, 13.234).some
    )
    fromTemplate("{{coord|11|08|S|27|06|E|type:adm1st_region:CD|display=inline,title}}") should ===(
      Coordinates(-11.133, 27.1).some
    )
    fromTemplate("{{coord|20|N|40|E|type:adm1st_region:CD|display=inline,title}}") should ===(
      Coordinates(20, 40).some
    )
    fromTemplate("{{coords|20|N|40|E|type:adm1st_region:CD|display=inline,title}}") should ===(
      Coordinates(20, 40).some
    )
    fromTemplate("{{coord|34|20|41|N|108|43|48|E|scale:10000|display=inline,title}}") should ===(
      Coordinates(34.345, 108.73).some
    )
  }

  it should "extract decimal lat and long when explicit" in {
    fromTemplate("{{coord|27.7177|85.3240|type:city_region:NP|display=it}}") should ===(
      Coordinates(27.718, 85.324).some
    )
    fromTemplate("{{coord|30|-60|type:city_region:NP|display=it}}") should ===(
      Coordinates(30, -60).some
    )
    fromTemplate("{{coord|46.995|N|120.549|W|region:US-WA|display=inline,title}}") should ===(
      Coordinates(46.995, -120.549).some
    )
    fromTemplate("{{coord|55.6666|S|22.3333|E|region:US-WA|display=inline,title}}") should ===(
      Coordinates(-55.667, 22.333).some
    )
    fromTemplate("{{coords|50.452222|23.396944|display=inline,title}}") should ===(
      Coordinates(50.452, 23.397).some
    )
  }

  it should "accept optional spaces between separators and values" in {
    fromTemplate("{{coord| 48.856613 | 2.352222 |type:city(2200000)_region:FR|format=dms|display=inline,title}}") should ===(
      Coordinates(48.857, 2.352).some
    )
    fromTemplate("{{coord   | 40 |25| 4|N | 85 |38|56 | W   | region:US-IN|display=inline,title}}") should ===(
      Coordinates(40.418, -85.649).some
    )
    fromTemplate("{{coords|50.452222| 23.396944|display=inline,title}}") should ===(
      Coordinates(50.452, 23.397).some
    )
  }

  it should "accept and ignore an arbitrary prefix and/or suffix in the coordinates string" in {
    fromTemplate("<!-- {{Coord}} -->{{coord|40|25|4|N|85|38|56|W|region:US-IN}}<ref name=gnis/> ") should ===(
      Coordinates(40.418, -85.649).some
    )
    fromTemplate("<!-- {{Coord}}<br><b>coord</b> -->{{coord|46.995|N|120.549|W|region:US-WA}}qwerty") should ===(
      Coordinates(46.995, -120.549).some
    )
  }

  it should "get coordinates also from any point of a raw page" in {
    val rawText = readFromFile("test_pages/former_country")
    fromTemplate(rawText) should ===(Coordinates(-55.667, 22.333).some)
  }
}
