package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.Validation

class String50 private (val value: String) extends AnyVal:
  override def toString = s"$value"

object String50:

  def make(value: String): Validation[String, String50] =
    if value.length <= 50 then Validation.succeed(new String50(value))
    else Validation.fail("Value exceeds 50")
