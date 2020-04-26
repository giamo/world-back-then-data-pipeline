package models.wikipedia

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class PageTitleTest extends AnyFlatSpec with Matchers {
  import PageTitle._

  "A page title" should "be extracted from a simple link, if present" in {
    fromLink("[[Lorem ipsum]]") should ===(PageTitle("Lorem ipsum").some)
    fromLink("* [[Rome, Italy]]* ") should ===(PageTitle("Rome, Italy").some)
    fromLink("[[   Rome, Italy ]]") should ===(PageTitle("Rome, Italy").some)
  }

  it should "be extracted correctly from a complex link" in {
    fromLink("[[Rome, Italy|Rome]]") should ===(PageTitle("Rome, Italy").some)
  }

  it should "be the extracted from the first link, when multiple are present" in {
    fromLink("[[Carmona]], now in [[Seville (province)|Seville]], [[Andalusia]], Spain") should ===(
      PageTitle("Carmona").some
    )
    fromLink("[[Carmona, Spain|Carmona]], now in [[Seville (province)|Seville]], [[Andalusia]], Spain") should ===(
      PageTitle("Carmona, Spain").some
    )
  }

  it should "not be extracted from malformed links" in {
    fromLink("[[Rome]") shouldBe empty
    fromLink("[Rome]]") shouldBe empty
    fromLink("[Rome]]") shouldBe empty
  }
}
