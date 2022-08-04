package io.gitlab.routis.dmmf.ordertaking.pub.internal

import io.gitlab.routis.dmmf.ordertaking.cmn.Common.*
import io.gitlab.routis.dmmf.ordertaking.pub.CheckAddressExists.{
  AddressValidationError,
  CheckedAddress
}
import io.gitlab.routis.dmmf.ordertaking.pub.PlaceOrder.*
import io.gitlab.routis.dmmf.ordertaking.pub.internal.PlaceOrderLive.{
  ValidatedOrder,
  ValidatedOrderLine
}
import io.gitlab.routis.dmmf.ordertaking.pub.internal.ValidatePlacedOrder.*
import io.gitlab.routis.dmmf.ordertaking.pub.internal.Validations.*
import io.gitlab.routis.dmmf.ordertaking.pub.internal.Validations.ValidationError.*
import io.gitlab.routis.dmmf.ordertaking.pub.{ CheckAddressExists, CheckProductCodeExists }
import zio.prelude.Validation
import zio.{ IO, NonEmptyChunk, UIO, URLayer, ZIO }

private[internal] case class ValidatePlacedOrder(
  checkAddressExists: CheckAddressExists,
  checkProductCodeExists: CheckProductCodeExists
) extends PlaceOrderLive.Validate:

  private def toCheckedAddress(
    field: FieldName,
    address: UnvalidatedAddress
  ): AsyncValidation[ValidationError, Address] =

    def toValidationErrors(e: AddressValidationError): NonEmptyChunk[ValidationError] =
      val description = e match
        case AddressValidationError.AddressNotFound => "Address not found"
        case AddressValidationError.InvalidFormat   => "Address invalid format"
      NonEmptyChunk.single(fieldError(field, description))

    ZIO.logSpan("validateAddress") {
      for
        present        <- ensurePresent(field, address).toAsync
        checkedAddress <- checkAddressExists.check(present).mapError(toValidationErrors)
        validAddress   <- toAddress.nest(field)(checkedAddress).toAsync
        _              <- ZIO.log("Valid")
      yield validAddress
    }

  private def toProductCode(
    field: FieldName,
    unvalidated: String
  ): AsyncValidation[ValidationError, ProductCode] =

    def exists(productCode: ProductCode) =
      ZIO
        .ifZIO(checkProductCodeExists.check(productCode))(
          onTrue = ZIO.succeed(productCode),
          onFalse = ZIO.fail(
            NonEmptyChunk
              .single(fieldError(field, s"Doesn't exist $productCode"))
          )
        )

    for
      productCode     <- makeProductCode.requiredField(field, unvalidated).toAsync
      existingProduct <- exists(productCode)
    yield existingProduct

  private def toValidatedOrderLine(
    unvalidated: UnvalidatedOrderLine
  ): AsyncValidation[ValidationError, ValidatedOrderLine] =
    type EitherVV[A] = Either[NonEmptyChunk[ValidationError], A]
    def assemble(
      maybeProductCode: EitherVV[ProductCode]
    ): Validation[ValidationError, ValidatedOrderLine] =
      val orderLineId =
        OrderLineId.make.requiredField("orderLineId", unvalidated.orderLineId)

      // if maybeProductCode is left then whatever the quantity we return failed
      // otherwise we check the quantity
      val quantityConstructor: SmartConstructor[Double, String, OrderQuantity] =
        maybeProductCode.fold(
          _ => _ => Validation.fail("Cannot be validated due to invalid product code"),
          pc => OrderQuantity.forProduct(pc)
        )

      val productCode = maybeProductCode.toValidation
      val quantity    = quantityConstructor.requiredField("quantity", unvalidated.quantity)
      Validation
        .validateWith(orderLineId, productCode, quantity)(ValidatedOrderLine.apply)

    for
      productCode <- toProductCode("productCode", unvalidated.productCode).either
      orderLine   <- assemble(productCode).toAsync
    yield orderLine

  def validateOrder(
    unvalidated: UnvalidatedOrder
  ): IO[PlaceOrderError.ValidationFailure, ValidatedOrder] =

    def lines(field: FieldName) =
      val maybeLines                                                  = Option(unvalidated.lines).flatMap(NonEmptyChunk.fromIterableOption)
      val ensureNotNullNotEmpty                                       =
        Validation.fromOptionWith(fieldError(field, "Missing or empty"))(maybeLines).toAsync
      def vol(unvalidatedOrderLine: UnvalidatedOrderLine, index: Int) =
        val errorMapper = indexFieldError(field, index)
        ZIO.logSpan("validateOrderLine") {
          ZIO.logAnnotate("index", s"$index") {
            toValidatedOrderLine(unvalidatedOrderLine)
              .mapError(es => es.map(errorMapper))
              .foldZIO(
                e => ZIO.log("Error") *> ZIO.fail(e),
                line => ZIO.log("Valid") *> ZIO.succeed(line)
              )
          }
        }

      (for
        nonEmptyLines   <- ensureNotNullNotEmpty
        linesValidation <- ZIO.collectAllPar(nonEmptyLines.zipWithIndex.map(vol))
      yield linesValidation).either

    type EitherNEC[A] = Either[NonEmptyChunk[ValidationError], A]
    def assemble(
      shippingAddress: EitherNEC[Address],
      billingAddress: EitherNEC[Address],
      lines: EitherNEC[NonEmptyChunk[ValidatedOrderLine]]
    ) =
      val orderId      = makeOrderId.requiredField("orderId", unvalidated.orderId)
      val customerInfo = toCustomerInfo.requiredField("customerInfo", unvalidated.customerInfo)
      Validation
        .validateWith(
          orderId,
          customerInfo,
          shippingAddress.toValidation,
          billingAddress.toValidation,
          lines.toValidation
        )(ValidatedOrder.apply)

    ZIO.logSpan("validateOrder") {
      ZIO.logAnnotate("orderId", unvalidated.orderId) {
        (for
          shippingAddressFiber <-
            toCheckedAddress("shippingAddress", unvalidated.shippingAddress).either.fork
          billingAddressFiber  <-
            toCheckedAddress("billingAddress", unvalidated.billingAddress).either.fork
          linesFiber           <- lines("line").fork
          shippingAddress      <- shippingAddressFiber.join
          billingAddress       <- billingAddressFiber.join
          lines                <- linesFiber.join
          validatedOrder       <- assemble(shippingAddress, billingAddress, lines).toAsync
          _                    <- ZIO.log("Valid")
        yield validatedOrder).mapError(PlaceOrderError.ValidationFailure.apply)
      }
    }

private object ValidatePlacedOrder:

  def toCustomerInfo(
    customerInfo: UnvalidatedCustomerInfo
  ): Validation[ValidationError, CustomerInfo] =
    Validation
      .validateWith(
        toPersonalName(customerInfo.firstName, customerInfo.lastName),
        makeEmailAddress.requiredField("emailAddress", customerInfo.emailAddress),
        makeVipStatus.requiredField("vipStatus", customerInfo.vipStatus)
      )(CustomerInfo.apply)

  def toAddress(checkedAddress: CheckedAddress): Validation[ValidationError, Address] =
    val addr = checkedAddress.unvalidatedAddress
    Validation
      .validateWith(
        makeString50.requiredField("addressLine1", addr.addressLine1),
        makeString50.optionalField("addressLine2", addr.addressLine2),
        makeString50.optionalField("addressLine3", addr.addressLine3),
        makeString50.optionalField("addressLine4", addr.addressLine4),
        makeString50.requiredField("city", addr.city),
        makeZipCode.requiredField("zipCode", addr.zipCode)
      )(Address.apply)

  def toPersonalName(
    firstName: String,
    lastName: String
  ): Validation[ValidationError, PersonalName] =
    Validation
      .validateWith(
        makeString50.requiredField("firstName", firstName),
        makeString50.requiredField("lastName", lastName)
      )(PersonalName.apply)
