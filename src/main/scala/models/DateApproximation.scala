package models

import cats.implicits._

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

  def fromString(
    approximationStr: String
  ): Either[DateParseError, DateApproximation] = approximationStr match {
    case null                                  => NONE.asRight
    case s if EarlyVariants.contains(s.trim)   => EARLY.asRight
    case s if MiddleVariants.contains(s.trim)  => MIDDLE.asRight
    case s if LateVariants.contains(s.trim)    => LATE.asRight
    case s if GenericVariants.contains(s.trim) => GENERIC.asRight
    case _ =>
      DateParseError(s"invalid approximation string: '$approximationStr'").asLeft

  }

}
