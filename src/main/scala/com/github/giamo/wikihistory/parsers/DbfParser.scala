package com.github.giamo.wikihistory.parsers

import java.io.InputStream
import java.nio.charset.Charset
import java.util.zip.ZipFile

import com.github.giamo.wikihistory.models.DbfRecord
import net.iryndin.jdbf.reader.DbfReader

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.Try
import scala.util.control.NonFatal

final class DbfParser(zipFilePath: String) {

  private val Utf8Charset: Charset = Charset.forName("utf8")

  def parse(): Option[List[DbfRecord]] = {
    getDbfInsideZip(zipFilePath).map { dbfInputStream =>
      try {
        val reader: DbfReader = new DbfReader(dbfInputStream)

        @tailrec
        def readRecords(acc: List[DbfRecord] = List()): List[DbfRecord] = {
          val record = reader.read()
          if (record == null) acc
          else {
            record.setStringCharset(Utf8Charset)
            val dbRecord = DbfRecord(
              year = fileYear().getOrElse(throw new Exception("error")),
              name = record.getString("NAME"),
              wikiPage = record.getString("WIKI_PAGE"),
              shortName = Option(record.getString("ABBREVNAME")),
              area = Option(record.getString("AREA")).map(_.toDouble)
            )
            readRecords(acc :+ dbRecord)
          }
        }

        readRecords()
      } catch {
        case NonFatal(ex) =>
          ex.printStackTrace()
          List()
      }
    }
  }

  private def getDbfInsideZip(zipFilePath: String): Option[InputStream] = {
    val rootZip = new ZipFile(zipFilePath)
    val zipEntries = rootZip.entries.asScala
    zipEntries.find(_.getName.endsWith(".dbf")).map { dbfEntry =>
      rootZip.getInputStream(dbfEntry)
    }
  }

  private val fileNameRegex = ".*/([\\-0-9]+)\\.zip".r
  def fileYear(): Option[Int] = zipFilePath match {
    case fileNameRegex(name) => Try(name.toInt).toOption
    case _ => None
  }
}
