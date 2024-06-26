package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude
import zio.prelude.{ Identity, Validation }

import scala.annotation.targetName
import scala.util.Try

import org.joda.money.Money as JodaMoney
import org.joda.money.CurrencyUnit as JodaCurrencyUnit

/**
 * [[Money]] represents a money amount for a specific currency.
 * The currency is fixed and cannot be changed at runtime
 */
opaque type Money = JodaMoney
object Money:

  def apply(amount: BigDecimal): Money = JodaMoney.of(JodaCurrencyUnit.EUR, amount.bigDecimal)

  def make(amount: BigDecimal): Validation[String, Money] =
    Validation
      .fromTry(Try(Money(amount)))
      .mapError(t => s"$amount is not a valid amount. ${t.getMessage}")

  def total(ms: Iterable[Money]): Money =
    import zio.prelude.*
    ms.reduceIdentity

  extension (self: Money)
    @targetName("+")
    def +(other: Money): Money = self.plus(other)
    @targetName("*")
    def *(times: Long): Money = self.multipliedBy(times)
    def amount: BigDecimal    = self.getAmount

  given MoneyAdditionIsIdentity: Identity[Money] with
    override def identity: Money                          = Money(0)
    override def combine(l: => Money, r: => Money): Money = l + r

  given MoneyIsOrdering: Ordering[Money] with
    override def compare(x: Money, y: Money): Int = x.compareTo(y)
