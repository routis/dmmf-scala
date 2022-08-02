package io.gitlab.routis.dmmf.ordertaking.cmn

import io.gitlab.routis.dmmf.ordertaking
import io.gitlab.routis.dmmf.ordertaking.cmn
import io.gitlab.routis.dmmf.ordertaking.cmn.Common.{ ProductCode, VipStatus }
import org.joda.money.{ BigMoney, MoneyUtils }

import java.util.Currency

object Common:
  import zio.prelude.Assertion.*
  import zio.prelude.{ Assertion, Newtype, Subtype, Validation }

  import scala.util.{ matching, Try }

  //
  // Simple types
  //

  object EmailAddress extends Subtype[String]:
    private val regex: matching.Regex                = "\\AW\\d{4}\\z".r
    override inline def assertion: Assertion[String] =
      matches(regex)
  type EmailAddress = EmailAddress.Type

  class String50 private (val value: String) extends AnyVal:
    override def toString = s"$value"
  object String50:
    def make(value: String): Validation[String, String50] =
      if value.length <= 50 then Validation.succeed(new String50(value))
      else Validation.fail("Value exceeds 50")

  object ZipCode extends Subtype[String]:
    private val regex: matching.Regex = "^\\d{5}$".r

    override inline def assertion: Assertion[String] =
      matches(regex)

  type ZipCode = String

  enum VipStatus extends java.lang.Enum[VipStatus]:
    case Normal, Vip
  object VipStatus:
    def make(name: String): Validation[String, VipStatus] =
      val res = name match
        case "Normal" => Option(Normal)
        case "Vip"    => Option(Vip)
        case _        => None
      Validation.fromOptionWith("Not a valid VipStatus")(res)

  enum ProductCode:
    case Widget(value: String) extends ProductCode
    case Gizmo(value: String) extends ProductCode

  object Kilograms extends Subtype[Double]:
    override inline def assertion: Assertion[Double] =
      greaterThanOrEqualTo(0d).&&(lessThanOrEqualTo(100d))
  type Kilograms = Kilograms.Type

  object Units extends Subtype[Int]:
    override inline def assertion: Assertion[Int] =
      greaterThanOrEqualTo(0).&&(lessThanOrEqualTo(100))

  type Units = Units.Type

  enum OrderQuantity:
    case KilogramsQ(quantity: Kilograms) extends OrderQuantity
    case UnitsQ(quantity: Units) extends OrderQuantity

  object OrderQuantity:

    def make(productCode: ProductCode, value: Double): Validation[String, OrderQuantity] =
      productCode match
        case _: ProductCode.Gizmo  =>
          Kilograms.make(value).map(KilogramsQ.apply(_))
        case _: ProductCode.Widget =>
          Units.make(value.intValue()).map(UnitsQ.apply(_))

    def value(orderQuantity: OrderQuantity): Double =
      orderQuantity match
        case KilogramsQ(k) => Kilograms.unwrap(k)
        case UnitsQ(u)     => Units.unwrap(u).toDouble

  object PromotionCode extends Newtype[String]

  type PromotionCode = PromotionCode.Type

  object OrderId extends Newtype[String]

  type OrderId = OrderId.Type

  object OrderLineId extends Newtype[String]

  type OrderLineId = OrderLineId.Type

  type Price         = BigDecimal
  type BillingAmount = BigDecimal

  //
  // Compound types
  //
  case class Address(
    addressLine1: String50,
    addressLine2: Option[String50],
    addressLine3: Option[String50],
    addressLine4: Option[String50],
    city: String50,
    zipCode: ZipCode
  )

  final case class PersonalName(fistName: String50, lastName: String50)

  final case class CustomerInfo(
    name: PersonalName,
    emailAddress: EmailAddress,
    vipStatus: VipStatus
  )

  object MoneyUtils:

    def makeAmount(
      currency: java.util.Currency,
      amount: BigDecimal
    ): zio.prelude.Validation[String, BigDecimal] =
      import org.joda.money.{ CurrencyUnit as JodaCurrencyUnit, Money as JodaMoney }
      Validation
        .fromTry(Try(JodaMoney.of(JodaCurrencyUnit.of(currency), amount.bigDecimal)))
        .mapError(t => s"Not a valid amount. ${t.getMessage}")
        .map(_.getAmount)

    def makeEuroAmount(amount: BigDecimal): zio.prelude.Validation[String, BigDecimal] =
      makeAmount(Currency.getInstance("EUR"), amount)

  @main
  def test(): Unit =
    println(MoneyUtils.makeEuroAmount(100.004))
