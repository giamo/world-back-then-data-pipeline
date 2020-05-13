package com.github.giamo.wikihistory.models

import java.util.regex.Pattern

import cats.implicits._

object DateApproximation {
  final val EarlyVariants = List("early")
  final val MiddleVariants = List("middle", "mid", "mid-")
  final val LateVariants = List("late")
  final val BeforeVariants = List("before")
  final val AfterVariants = List("after")
  final val GenericVariants = List("circa", "c.", "c", "ca.", "ca", "''circa''", "{{circa}}", "{{c.}}", "~")
  final val ApproximationVariantsStr: String =
    (EarlyVariants ++ MiddleVariants ++ LateVariants ++ BeforeVariants ++ AfterVariants ++ GenericVariants)
      .map(Pattern.quote)
      .mkString("|")

  sealed trait DateApproximation

  case object NONE extends DateApproximation

  case object GENERIC extends DateApproximation

  case object EARLY extends DateApproximation

  case object MIDDLE extends DateApproximation

  case object LATE extends DateApproximation

  case object BEFORE extends DateApproximation

  case object AFTER extends DateApproximation

  def fromString(
    approximationStr: String
  ): Either[DateParseError, DateApproximation] = approximationStr match {
    case null                                  => NONE.asRight
    case s if EarlyVariants.contains(s.trim)   => EARLY.asRight
    case s if MiddleVariants.contains(s.trim)  => MIDDLE.asRight
    case s if LateVariants.contains(s.trim)    => LATE.asRight
    case s if BeforeVariants.contains(s.trim)  => BEFORE.asRight
    case s if AfterVariants.contains(s.trim)   => AFTER.asRight
    case s if GenericVariants.contains(s.trim) => GENERIC.asRight
    case _                                     =>
      DateParseError(s"invalid approximation string: '$approximationStr'").asLeft

  }

}
