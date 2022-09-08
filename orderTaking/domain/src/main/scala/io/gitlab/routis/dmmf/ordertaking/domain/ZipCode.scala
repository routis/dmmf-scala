package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.{ Assertion, Subtype }
import zio.prelude.Assertion.matches

import scala.util.matching.Regex

object ZipCode extends Subtype[String]:
  override inline def assertion: Assertion[String] = matches("^\\d{5}$".r)
