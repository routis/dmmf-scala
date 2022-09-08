package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.{ Assertion, Subtype, Validation }
import Assertion.{ between, hasLength, matches }

import scala.util.matching.Regex

object String50 extends Subtype[String]:
  override inline def assertion: Assertion[String] = hasLength(between(1, 50))
