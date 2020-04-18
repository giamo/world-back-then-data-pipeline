package models

object DatingLabel {
  final val BCVariants = List("BC", "bc", "B.C.", "b.c.")
  final val ADVariants = List("AD", "ad", "A.D.", "a.d.")

  sealed trait DatingLabel {
    val label: String
  }

  case object BC extends DatingLabel {
    override val label = "BC"
  }

  case object AD extends DatingLabel {
    override val label = "BC"
  }

  def fromString(labelStr: String): Option[DatingLabel] = labelStr match {
    case s if BCVariants.contains(s) => Some(BC)
    case s if ADVariants.contains(s) => Some(AD)
    case _ => None
  }
}
