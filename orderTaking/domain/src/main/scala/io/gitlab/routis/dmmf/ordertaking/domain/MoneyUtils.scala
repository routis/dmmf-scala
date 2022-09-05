package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.Validation
import org.joda.money.{ CurrencyUnit as JodaCurrencyUnit, Money as JodaMoney }
import java.util.Currency

import scala.util.Try

object MoneyUtils:

  val EUR: JodaCurrencyUnit = JodaCurrencyUnit.EUR

  def of(currency: JodaCurrencyUnit, amount: BigDecimal): Validation[String, BigDecimal] =
    Validation
      .fromTry(Try(JodaMoney.of(currency, amount.bigDecimal)))
      .mapError(t => s"Not a valid amount. ${t.getMessage}")
      .map(_.getAmount)

  def of(currency: Currency, amount: BigDecimal): Validation[String, BigDecimal] =
    of(JodaCurrencyUnit.of(currency), amount)

  def eurosOf(amount: BigDecimal): Validation[String, BigDecimal] =
    of(EUR, amount)

  @main def test(): Unit =
    println(MoneyUtils.eurosOf(100.004))
    println(ProductCode.make("G123"))
