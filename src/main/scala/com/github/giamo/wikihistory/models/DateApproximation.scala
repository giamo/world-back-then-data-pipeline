package com.github.giamo.wikihistory.models

import java.util.regex.Pattern

import cats.implicits._

object DateApproximation {
  final val EarlyVariants = List("early")
  final val MiddleVariants = List("middle", "mid", "mid-")
  final val LateVariants = List("late")
  final val BeforeVariants = List("before")
  final val AfterVariants = List("after")
  final val GenericVariants = List("circa", "c.", "c", "ca.", "ca", "''circa''", "{{circa}}", "{{c.}}", "[[circa|c.]]", "~", "est", "est.")
  final val ApproximationVariantsStr: String =
    (EarlyVariants ++ MiddleVariants ++ LateVariants ++ BeforeVariants ++ AfterVariants ++ GenericVariants)
      .map(Pattern.quote)
      .mkString("|")

  sealed trait DateApproximation { def toPrettyString(): String }

  case object NONE extends DateApproximation { override def toPrettyString() = "" }

  case object GENERIC extends DateApproximation { override def toPrettyString() = "circa" }

  case object EARLY extends DateApproximation { override def toPrettyString() = "early" }

  case object MIDDLE extends DateApproximation { override def toPrettyString() = "middle" }

  case object LATE extends DateApproximation { override def toPrettyString() = "late" }

  case object BEFORE extends DateApproximation { override def toPrettyString() = "before" }

  case object AFTER extends DateApproximation { override def toPrettyString() = "after" }

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
