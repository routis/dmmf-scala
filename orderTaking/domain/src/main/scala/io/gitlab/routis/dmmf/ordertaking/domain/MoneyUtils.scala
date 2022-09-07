package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.Validation
import org.joda.money.{ CurrencyUnit as JodaCurrencyUnit, Money as JodaMoney }
import java.util.Currency

import scala.util.Try

object MoneyUtils:

  val EUR: JodaCurrencyUnit = JodaCurrencyUnit.EUR
  val zero: JodaMoney       = JodaMoney.zero(EUR)

  def makeUnsafe(amount: BigDecimal): JodaMoney = JodaMoney.of(EUR, amount.bigDecimal)
  def make(amount: BigDecimal): Validation[String, JodaMoney] =
    Validation
      .fromTry(Try(makeUnsafe(amount)))
      .mapError(t => s"Not a valid amount. ${t.getMessage}")

  inline given JodaMoneyHasOrdering: Ordering[JodaMoney] with
    override def compare(x: JodaMoney, y: JodaMoney): Int = x.compareTo(y)

  inline given JodaMoneyAdditionIsIdentity: zio.prelude.Identity[JodaMoney] with
    override def combine(l: => JodaMoney, r: => JodaMoney): JodaMoney = l.plus(r)

    override def identity: JodaMoney = zero
