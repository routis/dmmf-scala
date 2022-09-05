package io.gitlab.routis.dmmf.ordertaking.application.service

import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.*
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.PlaceOrderError.PricingError
import io.gitlab.routis.dmmf.ordertaking.application.port.out.{ CheckAddressExists, CheckProductCodeExists }
import io.gitlab.routis.dmmf.ordertaking.application.service.PlaceOrderService.*
import io.gitlab.routis.dmmf.ordertaking.domain.*
import zio.{ IO, NonEmptyChunk, UIO, URLayer, ZIO }

/**
 * The implementation of [PlaceOrder] Delegates validation to [Validate] which is implemented by [ValidatePlacedOrder]
 */
private case class PlaceOrderService(validationService: PlaceOrderValidationService) extends PlaceOrderUseCase:

  override def placeOrder(unvalidatedOrder: UnvalidatedOrder): IO[PlaceOrderError, List[PlaceOrderEvent]] =
    ???

object PlaceOrderService:

  //
  // Validation types and services
  //
  private[service] final case class ValidatedOrderLine(
    orderLineId: OrderLineId,
    productCode: ProductCode,
    quantity: OrderQuantity
  )

  private[service] final case class ValidatedOrder(
    orderId: OrderId,
    customerInfo: CustomerInfo,
    shippingAddress: Address,
    billingAddress: Address,
    lines: NonEmptyChunk[ValidatedOrderLine],
    pricingMethod: PricingMethod
  )

  private[service] trait PlaceOrderValidationService:
    def validateOrder(unvalidatedOrder: UnvalidatedOrder): IO[PlaceOrderError.ValidationFailure, ValidatedOrder]

  //
  // Pricing types and services
  //

  enum PricedOrderLine:
    case PricedOrderProductLine(
      orderLineId: OrderLineId,
      productCode: ProductCode,
      quantity: OrderQuantity,
      linePrice: Price
    )                             extends PricedOrderLine
    case Comment(comment: String) extends PricedOrderLine
  end PricedOrderLine

  enum PricingMethod:
    case Standard()
    case Promotion(code: PromotionCode)
  object PricingMethod:
    def create(promotionCode: String): PricingMethod = makePromotionCode
      .optional(Option(promotionCode))
      .fold(
        _ => Standard(),
        x =>
          x match
            case Some(pc) => Promotion(pc)
            case _        => Standard()
      )
  case class PricedOrder(
    orderId: OrderId,
    customerInfo: CustomerInfo,
    shippingAddress: Address,
    billingAddress: Address,
    amountToBill: BillingAmount,
    lines: NonEmptyChunk[PricedOrderLine],
    pricingMethod: PricingMethod
  )
  trait GetPrice:
    def getPrice(line: ValidatedOrderLine): IO[PricingError, PricedOrderLine]

  //
  // Dependency Injection
  //
  val layer: URLayer[CheckAddressExists & CheckProductCodeExists, PlaceOrderUseCase] =
    zio.ZLayer {
      for
        checkAddressExists     <- ZIO.service[CheckAddressExists]
        checkProductCodeExists <- ZIO.service[CheckProductCodeExists]
        validationService       = PlaceOrderValidationServiceLive(checkAddressExists, checkProductCodeExists)
      yield PlaceOrderService(validationService)
    }
