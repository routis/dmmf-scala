package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.{ Assertion, Subtype, Validation }
import Assertion.matches

import scala.util.matching.Regex

object String50 extends Subtype[String]:
  override inline def assertion: Assertion[String] = matches("^(?=[\\S\\s]{1,50}$)[\\S\\s]*".r)
