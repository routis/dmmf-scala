package io.gitlab.routis.dmmf.ordertaking.pub.internal

import io.gitlab.routis.dmmf.ordertaking.cmn.Common.ProductCode.value
import io.gitlab.routis.dmmf.ordertaking.dto.ValidationErrorDto
import io.gitlab.routis.dmmf.ordertaking.pub.PlaceOrder.{
  UnvalidatedAddress,
  UnvalidatedCustomerInfo,
  UnvalidatedOrder,
  UnvalidatedOrderLine
}
import io.gitlab.routis.dmmf.ordertaking.pub.internal.ValidatePlacedOrder.CheckAddressExists.CheckedAddress
import io.gitlab.routis.dmmf.ordertaking.pub.internal.ValidatePlacedOrder.{
  CheckAddressExists,
  CheckProductCodeExists
}
import zio.ZIO

object App extends zio.ZIOAppDefault:
  private val ethnikisAntistaseos =
    UnvalidatedAddress("Ethnikis Antistaseos 81A", "Vrilissia", null, null, "Athens", "15235")
  private val wrong               =
    ethnikisAntistaseos.copy(
      zipCode = ethnikisAntistaseos.zipCode + "  ",
      addressLine2 = " ".repeat(60)
    )

  private val line                              = UnvalidatedOrderLine("ol1", "G123", 10)
  private val order                             =
    UnvalidatedOrder(
      orderId = "oId123",
      unvalidatedCustomerInfo =
        UnvalidatedCustomerInfo("Babis", "Routis", "babis@yahoo.com", "Normal"),
      shippingAddress = ethnikisAntistaseos,
      billingAddress = ethnikisAntistaseos,
      lines = List(line, line.copy(quantity = -10)),
      // lines = List.empty,
      promotionCode = null
    )
  import zio.given
  val checkAddressExists: CheckAddressExists    = u =>
    ZIO.succeed(CheckedAddress(u)).delay(100.millisecond)
  val productCodeExists: CheckProductCodeExists = pc =>
    ZIO.succeed(value(pc) == "G123").delay(100.millisecond)

  // noinspection TypeAnnotation
  override def run =
    val placeOrder: ValidatePlacedOrder =
      ValidatePlacedOrder(checkAddressExists, productCodeExists)
    placeOrder
      .validateOrder(order)
      .mapError(ValidationErrorDto.fromDomain)
      .debug("here")
