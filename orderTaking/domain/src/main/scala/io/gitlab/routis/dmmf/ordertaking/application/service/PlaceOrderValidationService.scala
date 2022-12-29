package io.gitlab.routis.dmmf.ordertaking.application.service

import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.*
import io.gitlab.routis.dmmf.ordertaking.application.port.out.CheckAddressExists.{
  AddressValidationError,
  CheckedAddress
}
import io.gitlab.routis.dmmf.ordertaking.application.port.out.{ CheckAddressExists, CheckProductCodeExists }
import io.gitlab.routis.dmmf.ordertaking.application.service.PlaceOrderService.{
  PricingMethod,
  ValidatedOrder,
  ValidatedOrderLine
}
import io.gitlab.routis.dmmf.ordertaking.application.service.PlaceOrderValidationService.*
import io.gitlab.routis.dmmf.ordertaking.application.service.Validations.*
import io.gitlab.routis.dmmf.ordertaking.domain.*
import io.gitlab.routis.dmmf.ordertaking.domain.ValidationError.{
  ensurePresent,
  fieldError,
  indexFieldError,
  FieldName
}
import zio.prelude.*
import zio.{ IO, NonEmptyChunk, UIO, URLayer, ZIO }

import scala.util.Either

private[service] case class PlaceOrderValidationService(
  checkAddressExists: CheckAddressExists,
  checkProductCodeExists: CheckProductCodeExists
) extends PlaceOrderService.ValidateOrder:

  import PlaceOrderValidationService.DomainValidation

  private def toCheckedAddress(field: FieldName, address: UnvalidatedAddress): UIO[DomainValidation[Address]] =

    def toValidationErrors(e: AddressValidationError): ValidationError =
      val description = e match
        case AddressValidationError.AddressNotFound => "Address not found"
        case AddressValidationError.InvalidFormat   => "Address invalid format"
      fieldError(field, description)

    ZIO.logSpan("validateAddress") {
      (for
        present        <- ensurePresent(field, address).toIO
        checkedAddress <- checkAddressExists.check(present).mapError(e => NonEmptyChunk.single(toValidationErrors(e)))
        validAddress   <- toAddress.nest(field)(checkedAddress).toIO
        _              <- ZIO.log("Valid")
      yield validAddress).uioValidation
    }

  private def toProductCode(field: FieldName, unvalidated: String): UIO[DomainValidation[ProductCode]] =

    def exists(productCode: ProductCode): IO[NonEmptyChunk[ValidationError], ProductCode] =
      ZIO
        .ifZIO(checkProductCodeExists.check(productCode))(
          onTrue = ZIO.succeed(productCode),
          onFalse = ZIO.fail(
            NonEmptyChunk
              .single(fieldError(field, s"Doesn't exist $productCode"))
          )
        )

    (for
      productCode     <- makeProductCode.requiredField(field, unvalidated).toIO
      existingProduct <- exists(productCode)
    yield existingProduct).uioValidation

  private def toValidatedOrderLines(
    lines: List[UnvalidatedOrderLine]
  ): UIO[DomainValidation[NonEmptyChunk[ValidatedOrderLine]]] =

    val field: FieldName = "lines"

    def validateOrderLine(
      unvalidatedOrderLine: UnvalidatedOrderLine,
      index: Int
    ): UIO[DomainValidation[ValidatedOrderLine]] =

      val lineValidation =
        (for
          productCode <- toProductCode("productCode", unvalidatedOrderLine.productCode)
          orderLine   <- toValidatedOrderLine(unvalidatedOrderLine, productCode).toIO
        yield orderLine)
          .mapError(es => es.map(indexFieldError(field, index)))
      lineValidation.uioValidation
    end validateOrderLine

    val ensureNotNullNotEmpty: DomainValidation[NonEmptyChunk[UnvalidatedOrderLine]] =
      val maybeLines = Option(lines).flatMap(NonEmptyChunk.fromIterableOption)
      Validation.fromOptionWith(fieldError(field, "Missing or empty"))(maybeLines)

    ensureNotNullNotEmpty.fold(
      failure = es => ZIO.succeed(Validation.failNonEmptyChunk(es)),
      success = nonEmptyLines =>
        ZIO
          .collectAllPar(nonEmptyLines.zipWithIndex.map(validateOrderLine))
          .map(Validation.validateAll)
    )

  end toValidatedOrderLines

  def validateOrder(unvalidated: UnvalidatedOrder): IO[PlaceOrderError.ValidationFailure, ValidatedOrder] =

    val orderId      = makeOrderId.requiredField("orderId", unvalidated.orderId)
    val customerInfo = toCustomerInfo.requiredField("customerInfo", unvalidated.customerInfo)

    ZIO.logSpan("validateOrder") {
      ZIO.logAnnotate("orderId", unvalidated.orderId) {
        (for
          shippingAddressFiber <-
            toCheckedAddress("shippingAddress", unvalidated.shippingAddress).fork
          billingAddressFiber <-
            toCheckedAddress("billingAddress", unvalidated.billingAddress).fork
          linesFiber      <- toValidatedOrderLines(unvalidated.lines).fork
          shippingAddress <- shippingAddressFiber.join
          billingAddress  <- billingAddressFiber.join
          lines           <- linesFiber.join
          pricingMethod    = PricingMethod.create(unvalidated.promotionCode)
          _               <- ZIO.log("Valid")
        yield toValidatedOrder(
          orderId,
          customerInfo,
          shippingAddress,
          billingAddress,
          lines,
          pricingMethod
        )).ioValidation
          .mapError(PlaceOrderError.ValidationFailure.apply)
      }
    }

private[service] object PlaceOrderValidationService:
  type DomainValidation[A] = Validation[ValidationError, A]

  private def toValidatedOrder(
    orderId: Validation[ValidationError, OrderId],
    customerInfo: Validation[ValidationError, CustomerInfo],
    shippingAddress: Validation[ValidationError, Address],
    billingAddress: Validation[ValidationError, Address],
    lines: Validation[ValidationError, NonEmptyChunk[ValidatedOrderLine]],
    pricingMethod: PricingMethod
  ): DomainValidation[ValidatedOrder] =
    Validation
      .validateWith(orderId, customerInfo, shippingAddress, billingAddress, lines, Validation.succeed(pricingMethod))(
        ValidatedOrder.apply
      )

  private def toValidatedOrderLine(
    unvalidated: UnvalidatedOrderLine,
    productCode: Validation[ValidationError, ProductCode]
  ): DomainValidation[ValidatedOrderLine] =
    val orderLineId =
      makeOrderLineId.requiredField("orderLineId", unvalidated.orderLineId)

    // if maybeProductCode is left then whatever the quantity we return failed
    // otherwise we check the quantity
    val quantityConstructor: SmartConstructor[Double, String, OrderQuantity] =
      productCode.fold(
        _ => _ => Validation.fail("Cannot be validated due to invalid product code"),
        pc => OrderQuantity.forProduct(pc)
      )

    val quantity = quantityConstructor.requiredField("quantity", unvalidated.quantity)
    Validation
      .validateWith(orderLineId, productCode, quantity)(ValidatedOrderLine.apply)

  private def toCustomerInfo(customerInfo: UnvalidatedCustomerInfo): DomainValidation[CustomerInfo] =
    CustomerInfo.make(
      toPersonalName(customerInfo.firstName, customerInfo.lastName),
      makeEmailAddress.requiredField("emailAddress", customerInfo.emailAddress),
      makeVipStatus.requiredField("vipStatus", customerInfo.vipStatus)
    )

  private def toAddress(checkedAddress: CheckedAddress): DomainValidation[Address] =
    val addr = checkedAddress.unvalidatedAddress
    Address.make(
      makeString50.requiredField("addressLine1", addr.addressLine1),
      makeString50.optionalField("addressLine2", addr.addressLine2),
      makeString50.optionalField("addressLine3", addr.addressLine3),
      makeString50.optionalField("addressLine4", addr.addressLine4),
      makeString50.requiredField("city", addr.city),
      makeCountry.requiredField("country", addr.country),
      makeZipCode.requiredField("zipCode", addr.zipCode)
    )

  private def toPersonalName(firstName: String, lastName: String): DomainValidation[PersonalName] =
    PersonalName.make(
      makeString50.requiredField("firstName", firstName),
      makeString50.requiredField("lastName", lastName)
    )
