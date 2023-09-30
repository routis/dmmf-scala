package io.gitlab.routis.dmmf.ordertaking.application.service

import io.gitlab.routis.dmmf.ordertaking.Generators
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrder
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrder.UnvalidatedOrder
import io.gitlab.routis.dmmf.ordertaking.application.port.out.CheckAddressExists.CheckedAddress
import io.gitlab.routis.dmmf.ordertaking.application.port.out.{ CheckAddressExists, CheckProductCodeExists }
import io.gitlab.routis.dmmf.ordertaking.application.service.PlaceOrderService.ValidatedOrder
import io.gitlab.routis.dmmf.ordertaking.domain.OrderId
import zio.*
import zio.test.{ assertTrue, check, Spec, ZIOSpecDefault }

object ValidateOrderSpec extends ZIOSpecDefault:

  private val layer: ZLayer[Any, Nothing, PlaceOrderService.ValidateOrder] =
    val checkAddressExists: CheckAddressExists         = u => ZIO.succeed(CheckedAddress(u))
    val checkProductCodeExists: CheckProductCodeExists = pc => ZIO.succeed(true)
    ZLayer.succeed(ValidateOrder(checkAddressExists, checkProductCodeExists))

  private def validateOrder(
    unvalidatedOrder: UnvalidatedOrder
  ): ZIO[PlaceOrderService.ValidateOrder, PlaceOrder.PlaceOrderError.ValidationFailure, ValidatedOrder] =
    ZIO.serviceWithZIO[PlaceOrderService.ValidateOrder](_.apply(unvalidatedOrder))
  override def spec = suite("ValidateOrder")( //
    test("with valid input") {
      check(Generators.unvalidatedOrderGen) { order =>
        for validatedOrder <- validateOrder(order)
        yield assertTrue(OrderId.unwrap(validatedOrder.orderId) == order.orderId)
      }
    }
  ).provideLayerShared(Generators.faker ++ layer)
