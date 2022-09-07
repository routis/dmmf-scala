package io.gitlab.routis.dmmf.ordertaking.application.service

import io.gitlab.routis.dmmf.ordertaking.Application.Dto.PlaceOrderErrorDto
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.{
  UnvalidatedAddress,
  UnvalidatedCustomerInfo,
  UnvalidatedOrder,
  UnvalidatedOrderLine
}
import io.gitlab.routis.dmmf.ordertaking.application.port.out.CheckAddressExists.CheckedAddress
import io.gitlab.routis.dmmf.ordertaking.application.port.out.{
  CheckAddressExists,
  CheckProductCodeExists,
  GetPromotionProductPrice,
  GetStandardProductPrice
}

import io.gitlab.routis.dmmf.ordertaking.domain.{ MoneyUtils, Price, ProductCode, PromotionCode }
import zio.{ UIO, ZIO }

object App extends zio.ZIOAppDefault:

  private val standardPrices: GetStandardProductPrice = _ => ZIO.succeed(Price.makeUnsafe(10))

  private val promoPrices: GetPromotionProductPrice = (promoCode, _) =>
    promoCode match
      case PromotionCode("123") => ZIO.succeed(Some(Price.makeUnsafe(5)))
      case _                    => ZIO.succeed(None)

  private val ethnikisAntistaseos =
    UnvalidatedAddress("Ethnikis Antistaseos 81A", "Vrilissia", null, null, "Athens", "15235")
  private val wrong =
    ethnikisAntistaseos.copy(zipCode = ethnikisAntistaseos.zipCode + "  ", addressLine2 = " ".repeat(60))

  private val line = UnvalidatedOrderLine("ol1", "G123", 10)
  private val order =
    UnvalidatedOrder(
      orderId = "oId123",
      customerInfo = UnvalidatedCustomerInfo("Babis", "Routis", "babis@yahoo.com", "Normal"),
      shippingAddress = ethnikisAntistaseos,
      billingAddress = ethnikisAntistaseos,
      lines = List(line, line.copy(orderLineId = "old2", productCode = "G123")),
      // lines = List.empty,
      promotionCode = null
    )
  import zio.given

  lazy val placeOrderValidationSrv: PlaceOrderValidationService =
    val checkAddressExists: CheckAddressExists = u => ZIO.succeed(CheckedAddress(u)).delay(100.millisecond)
    val productCodeExists: CheckProductCodeExists = pc =>
      ZIO.succeed(ProductCode.value(pc) == "G123").delay(120.millisecond)
    PlaceOrderValidationService(checkAddressExists, productCodeExists)

  lazy val priceOrderSrv: PlaceOrderService.PriceOrder =
    PricingService(standardPrices, promoPrices)

  def placeOrder(unvalidatedOrder: UnvalidatedOrder): ZIO[Any, PlaceOrderErrorDto, PlaceOrderService.ValidatedOrder] =
    placeOrderValidationSrv
      .validateOrder(unvalidatedOrder)
      .mapError(PlaceOrderErrorDto.fromDomain)

  def priceOrder(
    validatedOrder: PlaceOrderService.ValidatedOrder
  ): ZIO[Any, PlaceOrderErrorDto, PlaceOrderService.PricedOrder] =
    priceOrderSrv
      .priceOrder(validatedOrder)
      .mapError(PlaceOrderErrorDto.fromDomain)

  // noinspection TypeAnnotation
  override def run =
    placeOrder(order).flatMap(priceOrder).either.debug("here")
