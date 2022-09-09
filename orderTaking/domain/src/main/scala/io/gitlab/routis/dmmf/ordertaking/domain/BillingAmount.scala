package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.{ Assertion, Identity, Subtype, Validation }
import Assertion.*
import zio.prelude.newtypes.Sum

object BillingAmount extends Subtype[Money]:

  override inline def assertion: Assertion[Money] = between(Money(0), Money(10000))

  def total(prices: Iterable[Price]): Validation[String, BillingAmount] =
    make(Money.total(prices))
