package models.wikipedia

import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class PageTitleTest extends AnyFlatSpec with Matchers {
  import PageTitle._

  "A page title" should "be extracted from a simple link, if present" in {
    fromLink("[[Lorem ipsum]]") should ===(PageTitle("Lorem ipsum"))
    fromLink("simple text") should ===(PageTitle("simple text"))
  }

  it should "be extracted from a complex link" in {
    fromLink("[[Lorem|Lorem ipsum]]") should ===(PageTitle("Lorem ipsum"))
  }
}
