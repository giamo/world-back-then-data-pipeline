package com.github.giamo.wikihistory.models.wikipedia

final case class IndexRecord(pageId: Long, pageTitle: String)

object IndexRecord {
  def apply(record: String): IndexRecord = {
    val parts = record.split(":")
    IndexRecord(parts(1).toLong, parts.drop(2).mkString(":"))
  }
}
