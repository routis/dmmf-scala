package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.*
import zio.prelude.Assertion.*

import java.util.Locale
import java.util.Locale.getISOCountries

object Iso3166:

  object Part1Alpha2 extends NewtypeCustom[String]:
    protected def validate(value: String): Either[AssertionError, Unit] =
      Part1Alpha2Validator.validate(value)

    inline def validateInline(inline value: String): Unit =
      ${ Part1Alpha2Validator.validateInlineImpl('{ value }) }

  object Part1Alpha2Validator
      extends zio.prelude.Validator[String](str =>
        if str.length == 2 && Locale.getISOCountries(Locale.IsoCountryCode.PART1_ALPHA2).contains(str) then Right(())
        else Left(AssertionError.failure("isIso3166Part1Alpha2"))
      )
