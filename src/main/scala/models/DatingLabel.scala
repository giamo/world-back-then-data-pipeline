package models

import cats.implicits._

object DatingLabel {
  final val BCVariants =
    List("BC", "bc", "B.C.", "b.c.", "BCE", "bce", "B.C.E.", "b.c.e.")
  final val ADVariants =
    List("AD", "ad", "A.D.", "a.d.", "CE", "ce", "C.E.", "c.e.")

  sealed trait DatingLabel {
    val label: String
  }

  case object BC extends DatingLabel {
    override val label = "BC"
  }

  case object AD extends DatingLabel {
    override val label = "BC"
  }

  def fromString(labelStr: String): Either[DateParseError, DatingLabel] =
    labelStr match {
      case s if s == null || ADVariants.contains(s.trim) => AD.asRight
      case s if BCVariants.contains(s.trim)              => BC.asRight
      case _ =>
        DateParseError(s"invalid dating label string: '$labelStr'").asLeft
    }
}
