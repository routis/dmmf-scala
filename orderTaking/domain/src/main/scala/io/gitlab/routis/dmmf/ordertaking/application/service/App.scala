package io.gitlab.routis.dmmf.ordertaking.application.service

import io.gitlab.routis.dmmf.ordertaking.Application.Dto.PlaceOrderErrorDto
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.{
  UnvalidatedAddress,
  UnvalidatedCustomerInfo,
  UnvalidatedOrder,
  UnvalidatedOrderLine
}
import io.gitlab.routis.dmmf.ordertaking.application.port.out.CheckAddressExists.CheckedAddress
import io.gitlab.routis.dmmf.ordertaking.application.port.out.{ CheckAddressExists, CheckProductCodeExists }
import io.gitlab.routis.dmmf.ordertaking.domain.ProductCode.value
import zio.ZIO

object App extends zio.ZIOAppDefault:
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
      lines = List(line, line.copy(orderLineId = "old2", productCode = "W123")),
      // lines = List.empty,
      promotionCode = null
    )
  import zio.given

  lazy val placeOrderSrv: PlaceOrderValidationServiceLive =
    val checkAddressExists: CheckAddressExists    = u => ZIO.succeed(CheckedAddress(u)).delay(100.millisecond)
    val productCodeExists: CheckProductCodeExists = pc => ZIO.succeed(value(pc) == "G123").delay(120.millisecond)
    PlaceOrderValidationServiceLive(checkAddressExists, productCodeExists)

  def placeOrder(unvalidatedOrder: UnvalidatedOrder): ZIO[Any, PlaceOrderErrorDto, PlaceOrderService.ValidatedOrder] =
    placeOrderSrv
      .validateOrder(unvalidatedOrder)
      .mapError(PlaceOrderErrorDto.fromDomain)

  // noinspection TypeAnnotation
  override def run =
    placeOrder(order).either.debug("here")
