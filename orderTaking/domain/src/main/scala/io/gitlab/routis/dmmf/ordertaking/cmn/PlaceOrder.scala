package io.gitlab.routis.dmmf.ordertaking.cmn
import zio.{ IO, ZIO }

trait PlaceOrder:
  import PlaceOrder.{ PlaceOrderError, PlaceOrderEvents, UnvalidatedOrder }
  def placeOrder(unvalidatedOrder: UnvalidatedOrder): IO[PlaceOrderError, PlaceOrderEvents]

object PlaceOrder:

  case class UnvalidatedCustomerInfo(
    firstName: String,
    lastName: String,
    emailAddress: String,
    vipStatus: String
  )

  case class UnvalidatedAddress(
    addressLine1: String,
    addressLine2: String,
    addressLine3: String,
    addressLine4: String,
    city: String,
    zipCode: String
  )

  case class UnvalidatedOrderLine(orderLineId: String, productCode: String, quantity: Double)

  case class UnvalidatedOrder(
    orderId: String,
    unvalidatedCustomerInfo: UnvalidatedCustomerInfo,
    shippingAddress: UnvalidatedAddress,
    billingAddress: UnvalidatedAddress,
    lines: List[UnvalidatedOrderLine],
    promotionCode: String
  )

  import io.gitlab.routis.dmmf.ordertaking.cmn.Common.{
    Address,
    BillingAmount,
    EmailAddress,
    OrderId,
    OrderQuantity,
    ProductCode,
    PromotionCode
  }

  case class ShippableOrderLine(productCode: ProductCode, quantity: OrderQuantity)

  enum PlaceOrderEvents:

    case ShippableOrderSent(
      orderId: OrderId,
      shippingAddress: Address,
      billingAddress: Address,
      lines: List[ShippableOrderLine],
      promotionCode: PromotionCode
    ) extends PlaceOrderEvents

    case BillableOrderPlaced(
      orderId: OrderId,
      billingAddress: Address,
      billingAmount: BillingAmount
    ) extends PlaceOrderEvents

    case AcknowledgementSent(orderId: OrderId, emailAddress: EmailAddress) extends PlaceOrderEvents

  end PlaceOrderEvents

  import Validations.ValidationError
  enum PlaceOrderError:

    case ValidationFailure(errors: zio.prelude.NonEmptyList[ValidationError])
        extends PlaceOrderError
    case PricingError(cause: String) extends PlaceOrderError
  end PlaceOrderError
