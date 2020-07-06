package com.github.giamo.wikihistory.models.wikipedia

final case class ParsedPage(
  pageId: Long,
  pageTitle: String,
  synopsis: String,
)

object ParsedPage {
  def fromRawPage(page: WikiPage) =
    ParsedPage(
      pageId = page.id,
      pageTitle = page.title,
      synopsis = WikiPage.getHtmlSynopsis(page.text)
    )
}
