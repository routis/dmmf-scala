package io.gitlab.routis.dmmf.ordertaking.pub.internal

import io.gitlab.routis.dmmf.ordertaking.cmn.ProductCode.value
import io.gitlab.routis.dmmf.ordertaking.pub.CheckProductCodeExists
import io.gitlab.routis.dmmf.ordertaking.pub.PlaceOrder.{
  UnvalidatedAddress,
  UnvalidatedCustomerInfo,
  UnvalidatedOrder,
  UnvalidatedOrderLine
}
import io.gitlab.routis.dmmf.ordertaking.pub.CheckAddressExists.CheckedAddress
import io.gitlab.routis.dmmf.ordertaking.pub.CheckAddressExists
import io.gitlab.routis.dmmf.ordertaking.Application.Dto.{ PlaceOrderErrorDto, ValidationErrorDto }
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

  lazy val placeOrderSrv: ValidatePlacedOrder =
    val checkAddressExists: CheckAddressExists    = u => ZIO.succeed(CheckedAddress(u)).delay(100.millisecond)
    val productCodeExists: CheckProductCodeExists = pc => ZIO.succeed(value(pc) == "G123").delay(120.millisecond)
    ValidatePlacedOrder(checkAddressExists, productCodeExists)

  def placeOrder(unvalidatedOrder: UnvalidatedOrder): ZIO[Any, PlaceOrderErrorDto, PlaceOrderLive.ValidatedOrder] =
    placeOrderSrv
      .validateOrder(unvalidatedOrder)
      .mapError(PlaceOrderErrorDto.fromDomain)

  // noinspection TypeAnnotation
  override def run =
    placeOrder(order).either.debug("here")
