package models

object DateApproximation {
  final val EarlyVariants = List("early")
  final val MiddleVariants = List("middle")
  final val LateVariants = List("late")
  final val GenericVariants = List("circa", "c.", "c", "ca.", "ca")

  sealed trait DateApproximation

  case object NONE extends DateApproximation

  case object GENERIC extends DateApproximation

  case object EARLY extends DateApproximation

  case object MIDDLE extends DateApproximation

  case object LATE extends DateApproximation

  def fromString(approximationStr: String): Option[DateApproximation] = approximationStr match {
    case null => None
    case s if EarlyVariants.contains(s.trim) => Some(EARLY)
    case s if MiddleVariants.contains(s.trim) => Some(MIDDLE)
    case s if LateVariants.contains(s.trim) => Some(LATE)
    case s if GenericVariants.contains(s.trim) => Some(GENERIC)
    case _ => None

  }

}
