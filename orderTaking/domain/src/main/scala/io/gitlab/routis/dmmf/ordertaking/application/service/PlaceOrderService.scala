package io.gitlab.routis.dmmf.ordertaking.application.service

import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.*
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.PlaceOrderError.PricingError
import io.gitlab.routis.dmmf.ordertaking.application.port.out.{
  CheckAddressExists,
  CheckProductCodeExists,
  GetPromotionProductPrice,
  GetStandardProductPrice
}
import io.gitlab.routis.dmmf.ordertaking.application.service.PlaceOrderService.*
import io.gitlab.routis.dmmf.ordertaking.domain.*
import zio.{ IO, NonEmptyChunk, UIO, URLayer, ZIO }

/**
 * The implementation of [PlaceOrder] Delegates validation to [Validate] which is implemented by [ValidatePlacedOrder]
 */
private[service] case class PlaceOrderService(validationService: ValidateOrder, priceService: PriceOrder)
    extends PlaceOrderUseCase:

  override def placeOrder(unvalidatedOrder: UnvalidatedOrder): IO[PlaceOrderError, List[PlaceOrderEvent]] =
    for
      validatedOrder <- validationService.validateOrder(unvalidatedOrder)
      pricedOrder    <- priceService.priceOrder(validatedOrder)
    yield List.empty

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

  private[service] trait ValidateOrder:
    def validateOrder(unvalidatedOrder: UnvalidatedOrder): IO[PlaceOrderError.ValidationFailure, ValidatedOrder]

  //
  // Pricing types and services
  //

  enum PricedOrderLine:

    def price: Price =
      this match
        case PricedOrderProductLine(_, _, _, linePrice) => linePrice
        case Comment(_)                                 => Price.zero

    case PricedOrderProductLine(
      orderLineId: OrderLineId,
      productCode: ProductCode,
      quantity: OrderQuantity,
      linePrice: Price
    )                             extends PricedOrderLine
    case Comment(comment: String) extends PricedOrderLine
  end PricedOrderLine

  enum PricingMethod:
    case Standard()                     extends PricingMethod
    case Promotion(code: PromotionCode) extends PricingMethod
  object PricingMethod:
    def create(promotionCode: String): PricingMethod =
      Option(promotionCode)
        .filter(_.nonEmpty)
        .flatMap(it => PromotionCode.make(it).toOption.map(Promotion.apply))
        .getOrElse(Standard())

  case class PricedOrder(
    orderId: OrderId,
    customerInfo: CustomerInfo,
    shippingAddress: Address,
    billingAddress: Address,
    amountToBill: BillingAmount,
    lines: NonEmptyChunk[PricedOrderLine],
    pricingMethod: PricingMethod
  )
  private[service] trait PriceOrder:
    def priceOrder(validatedOrder: ValidatedOrder): IO[PricingError, PricedOrder]

  //
  // Dependency Injection
  //
  val layer: URLayer[
    CheckAddressExists & CheckProductCodeExists & GetStandardProductPrice & GetPromotionProductPrice,
    PlaceOrderUseCase
  ] =
    zio.ZLayer {
      for
        checkAddressExists     <- ZIO.service[CheckAddressExists]
        checkProductCodeExists <- ZIO.service[CheckProductCodeExists]
        standardPrices         <- ZIO.service[GetStandardProductPrice]
        promoPrices            <- ZIO.service[GetPromotionProductPrice]
        validationService       = PlaceOrderValidationService(checkAddressExists, checkProductCodeExists)
        priceService            = PricingService(standardPrices, promoPrices)
      yield PlaceOrderService(validationService, priceService)
    }
