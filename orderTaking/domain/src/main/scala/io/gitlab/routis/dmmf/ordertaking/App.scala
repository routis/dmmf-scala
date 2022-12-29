package io.gitlab.routis.dmmf.ordertaking

import io.gitlab.routis.dmmf.ordertaking.OrderService
import io.gitlab.routis.dmmf.ordertaking.OrderService.Dto
import io.gitlab.routis.dmmf.ordertaking.OrderService.Dto.PlaceOrderErrorDto
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.{UnvalidatedAddress, UnvalidatedCustomerInfo, UnvalidatedOrder, UnvalidatedOrderLine}
import io.gitlab.routis.dmmf.ordertaking.application.port.out.CheckAddressExists.CheckedAddress
import io.gitlab.routis.dmmf.ordertaking.application.port.out.{CheckAddressExists, CheckProductCodeExists, GetPromotionProductPrice, GetStandardProductPrice}
import io.gitlab.routis.dmmf.ordertaking.application.service.PlaceOrderService
import io.gitlab.routis.dmmf.ordertaking.domain.{Price, ProductCode, PromotionCode}
import zio.*

object App extends zio.ZIOAppDefault:

  private lazy val standardPricesLayer: ULayer[GetStandardProductPrice] =
    val standardPrices: GetStandardProductPrice = _ => ZIO.succeed(Price(10))
    ZLayer.succeed(standardPrices)

  private lazy val promoPricesLayer: ULayer[GetPromotionProductPrice] =
    val promoPrices: GetPromotionProductPrice = (promoCode, _) =>
      promoCode match
        case PromotionCode("123") => ZIO.succeed(Some(Price(5)))
        case _                    => ZIO.succeed(None)
    ZLayer.succeed(promoPrices)

  private lazy val checkAddressExistsLayer: ULayer[CheckAddressExists] =
    ZLayer.succeed { u =>
      ZIO.succeed(CheckedAddress(u)).delay(100.millisecond)
    }
  private lazy val checkProductCodeExistsLayer: ULayer[CheckProductCodeExists] =
    ZLayer.succeed { pc =>
      ZIO.succeed(pc.value == "G123").delay(120.millisecond)
    }

  private val sampleAddress =
    Dto.AddressDto("Ethnikis Antistaseos 81A", "Vrilissia", null, null, "Athens", "GR", "15235")
  private val wrong =
    sampleAddress.copy(zipCode = sampleAddress.zipCode + "  ", addressLine2 = " ".repeat(60))

  private val line = Dto.OrderLineDto("ol1", "G123", 10)
  private val order =
    Dto.OrderDto(
      orderId = "oId123",
      customerInfo = Dto.CustomerInfoDto("Babis", "Routis", "babis@yahoo.com", "Normal"),
      shippingAddress = sampleAddress,
      billingAddress = sampleAddress,
      lines = Array(line, line.copy(orderLineId = "old2", productCode = "G123")),
      promotionCode = null
    )
 // import zio.given

  // noinspection TypeAnnotation
  override def run =
    OrderService
      .placeOrder(order)
      .either
      .debug("here")
      .provide(
        checkProductCodeExistsLayer,
        checkAddressExistsLayer,
        standardPricesLayer,
        promoPricesLayer,
        PlaceOrderService.layer,
        OrderService.live
      )
