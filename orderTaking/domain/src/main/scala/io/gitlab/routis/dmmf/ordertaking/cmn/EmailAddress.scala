package io.gitlab.routis.dmmf.ordertaking.cmn

import zio.prelude.{ Assertion, Subtype }
import zio.prelude.Assertion.matches

import scala.util.matching.Regex

object EmailAddress extends Subtype[String]:
  val regex: Regex = "^(.+)@(.+)$".r

  override inline def assertion: Assertion[String] =
    matches(regex)
