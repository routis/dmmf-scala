package io.gitlab.routis.dmmf.ordertaking.cmn

import zio.prelude.Validation
import org.joda.money.{ CurrencyUnit as JodaCurrencyUnit, Money as JodaMoney }
import java.util.Currency

import scala.util.Try

object MoneyUtils:

  val EUR: JodaCurrencyUnit = JodaCurrencyUnit.EUR

  def makeAmount(currency: JodaCurrencyUnit, amount: BigDecimal): Validation[String, BigDecimal] =
    Validation
      .fromTry(Try(JodaMoney.of(currency, amount.bigDecimal)))
      .mapError(t => s"Not a valid amount. ${t.getMessage}")
      .map(_.getAmount)

  def makeAmount(currency: Currency, amount: BigDecimal): Validation[String, BigDecimal] =
    makeAmount(JodaCurrencyUnit.of(currency), amount)

  def makeEuroAmount(amount: BigDecimal): Validation[String, BigDecimal] =
    makeAmount(EUR, amount)

  @main def test(): Unit =
    println(MoneyUtils.makeEuroAmount(100.004))
    println(ProductCode.make("G123"))
