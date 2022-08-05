package io.gitlab.routis.dmmf.ordertaking.cmn

import zio.prelude.{ Assertion, Subtype }
import zio.prelude.Assertion.matches

import scala.util.matching.Regex

object ZipCode extends Subtype[String]:
  val regex: Regex = "^\\d{5}$".r

  override inline def assertion: Assertion[String] = matches(regex)
