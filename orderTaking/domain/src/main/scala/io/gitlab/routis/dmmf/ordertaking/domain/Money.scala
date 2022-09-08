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

  val zero: Money                      = Money(0)
  def apply(amount: BigDecimal): Money = JodaMoney.of(JodaCurrencyUnit.EUR, amount.bigDecimal)

  def make(amount: BigDecimal): Validation[String, Money] =
    Validation
      .fromTry(Try(Money(amount)))
      .mapError(t => s"Not a valid amount. ${t.getMessage}")

  extension (self: Money)
    @targetName("+")
    def +(other: Money): Money = self.plus(other)
    @targetName("*")
    def *(times: Long): Money = self.multipliedBy(times)

  given MoneyAdditionIsIdentity: Identity[Money] with
    override def identity: Money = zero

    override def combine(l: => Money, r: => Money): Money = l + r

  given MoneyHasOrdering: Ordering[Money] with
    override def compare(l: Money, r: Money): Int = l.compareTo(r)
