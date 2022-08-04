package io.gitlab.routis.dmmf.ordertaking.cmn

import io.gitlab.routis
import io.gitlab.routis.dmmf
import io.gitlab.routis.dmmf.ordertaking
import io.gitlab.routis.dmmf.ordertaking.cmn
import io.gitlab.routis.dmmf.ordertaking.cmn.Common.{ ProductCode, VipStatus }
import org.joda.money.{ BigMoney, MoneyUtils }

import java.util.Currency

object Common:
  import zio.prelude.Assertion.*
  import zio.prelude.newtypes.Sum
  import zio.prelude.{ Assertion, Newtype, Subtype, Validation }

  import scala.util.{ matching, Try }

  def makeEmailAddress(u: String): Validation[String, EmailAddress] = EmailAddress.make(u)

  def makeString50(u: String): Validation[String, String50] = String50.make(u)

  def makeZipCode(u: String): Validation[String, ZipCode] = ZipCode.make(u)

  def makeVipStatus(u: String): Validation[String, VipStatus] = VipStatus.make(u)

  def makeProductCode(u: String): Validation[String, ProductCode] = ProductCode.make(u)

  def makeOrderQuantity(productCode: ProductCode)(
    quantity: Double
  ): Validation[String, OrderQuantity] =
    OrderQuantity.forProduct(productCode)(quantity)
  def makeOrderId(u: String): Validation[String, OrderId] = OrderId.make(u)

  def makeOrderLineId(u: String): Validation[String, OrderLineId] = OrderLineId.make(u)

  //
  // Simple types
  //

  object EmailAddress extends Subtype[String]:
    private val regex: matching.Regex                = "^(.+)@(.+)$".r
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
    private val regex: matching.Regex                = "^\\d{5}$".r
    override inline def assertion: Assertion[String] = matches(regex)
  type ZipCode = ZipCode.Type

  enum VipStatus extends java.lang.Enum[VipStatus]:
    case Normal, Vip
  object VipStatus:
    def make(name: String): Validation[String, VipStatus] =
      val res = name match
        case "Normal" => Option(Normal)
        case "Vip"    => Option(Vip)
        case _        => None
      Validation.fromOptionWith("Not a valid VipStatus")(res)

  object Widget extends Subtype[String]:
    override inline def assertion: Assertion[String] =
      matches("\\AW\\d{4}\\z".r)
  type Widget = Widget.Type
  object Gizmo  extends Subtype[String]:
    override inline def assertion: Assertion[String] =
      matches("\\AG\\d{3}\\z".r)
  type Gizmo = Gizmo.Type

  sealed trait ProductCode
  object ProductCode:
    case class GizmoCode(gizmo: Gizmo) extends ProductCode

    case class WidgetCode(widget: Widget) extends ProductCode
    def value(productCode: ProductCode): String              =
      productCode match
        case GizmoCode(gizmo)   => Gizmo.unwrap(gizmo)
        case WidgetCode(widget) => Widget.unwrap(widget)
    def make(value: String): Validation[String, ProductCode] =
      Widget
        .make(value)
        .map(WidgetCode.apply)
        .orElse(Gizmo.make(value).map(GizmoCode.apply))
        .mapError(_ => "Neither Widget nor Gizmo")

  object Kilograms extends Subtype[Double]:
    override inline def assertion: Assertion[Double] =
      greaterThanOrEqualTo(0d).&&(lessThanOrEqualTo(100d))
  type Kilograms = Kilograms.Type

  object Units extends Subtype[Int]:
    override inline def assertion: Assertion[Int] =
      greaterThanOrEqualTo(0).&&(lessThanOrEqualTo(100))

  type Units = Units.Type

  type OrderQuantity = Kilograms | Units
  object OrderQuantity:
    def forProduct(productCode: ProductCode)(value: Double): Validation[String, OrderQuantity] =
      productCode match
        case _: ProductCode.GizmoCode  =>
          Kilograms.make(value)
        case _: ProductCode.WidgetCode =>
          Units.make(value.intValue())

    inline def value(orderQuantity: OrderQuantity): Double =
      orderQuantity match
        case Kilograms(k) => k
        case Units(u)     => u.toDouble

  object PromotionCode extends Newtype[String]

  type PromotionCode = PromotionCode.Type

  object OrderId extends Newtype[String]

  type OrderId = OrderId.Type

  object OrderLineId extends Newtype[String]

  type OrderLineId = OrderLineId.Type

  type Price         = BigDecimal
  type BillingAmount = zio.prelude.newtypes.Sum[BigDecimal]

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
    println(ProductCode.make("G123"))
