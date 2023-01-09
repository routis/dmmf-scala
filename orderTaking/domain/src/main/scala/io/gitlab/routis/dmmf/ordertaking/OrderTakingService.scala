package io.gitlab.routis.dmmf.ordertaking

import zio.{ IO, ULayer, ZLayer }
import zio.prelude.Validation

trait OrderTakingService:

  import OrderTakingService.Dto.{ OrderDto, PlaceOrderErrorDto }
  def placeOrder(order: OrderDto): IO[PlaceOrderErrorDto, Unit]

object OrderTakingService:

  import OrderTakingService.Dto.PlaceOrderErrorDto
  import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrder
  import zio.{ URLayer, ZIO, ZLayer }
  private case class OrderTakingServiceLive(private val placeOrder: PlaceOrder) extends OrderTakingService:
    override def placeOrder(order: Dto.OrderDto): IO[Dto.PlaceOrderErrorDto, Unit] =
      (for
        unvalidatedOrder <- ZIO.succeed(order.toUnvalidated)
        events           <- placeOrder.execute(unvalidatedOrder).mapError(PlaceOrderErrorDto.fromDomain)
        _ <- ZIO.log(s"Events : $events")
      yield events).unit

  lazy val live: URLayer[PlaceOrder, OrderTakingService] =
    ZLayer {
      for placeOrderUseCase <- ZIO.service[PlaceOrder]
      yield OrderTakingServiceLive(placeOrderUseCase)
    }
  def placeOrder(order: Dto.OrderDto): ZIO[OrderTakingService, Dto.PlaceOrderErrorDto, Unit] =
    ZIO.serviceWithZIO[OrderTakingService](_.placeOrder(order))

  object Dto:

    import io.gitlab.routis.dmmf.ordertaking.domain.*
    import PlaceOrder.*
    import ValidationError.{ Cause, FieldError, IndexedFieldError }
    import PlaceOrderError.{ PricingError, ValidationFailure }

    case class CustomerInfoDto(firstName: String, lastName: String, emailAddress: String, vipStatus: String)
    object CustomerInfoDto:
      def fromDomain(domain: CustomerInfo): CustomerInfoDto =
        CustomerInfoDto(
          firstName = domain.name.fistName,
          lastName = domain.name.lastName,
          emailAddress = EmailAddress.unwrap(domain.emailAddress),
          vipStatus = domain.vipStatus.name
        )

      extension (dto: CustomerInfoDto)
        def toUnvalidated: UnvalidatedCustomerInfo =
          UnvalidatedCustomerInfo(
            firstName = dto.firstName,
            lastName = dto.lastName,
            emailAddress = dto.emailAddress,
            vipStatus = dto.vipStatus
          )
        def toDomain: Validation[ValidationError, CustomerInfo] =
          def makePersonalName =
            for
              firstName <- makeString50.requiredField("firstName", dto.firstName)
              lastName  <- makeString50.requiredField("lastName", dto.lastName)
            yield PersonalName(firstName, lastName)

          for
            name         <- makePersonalName
            emailAddress <- makeEmailAddress.requiredField("emailAddress", dto.emailAddress)
            vipStatus    <- makeVipStatus.requiredField("vipStatus", dto.vipStatus)
          yield CustomerInfo(name, emailAddress, vipStatus)
    end CustomerInfoDto

    case class AddressDto(
      addressLine1: String,
      addressLine2: String,
      addressLine3: String,
      addressLine4: String,
      city: String,
      country: String,
      zipCode: String
    )
    object AddressDto:
      def fromDomain(domain: Address): AddressDto =
        AddressDto(
          addressLine1 = domain.addressLine1,
          addressLine2 = domain.addressLine2.orNull,
          addressLine3 = domain.addressLine3.orNull,
          addressLine4 = domain.addressLine4.orNull,
          city = domain.city,
          country = domain.country,
          zipCode = ZipCode.unwrap(domain.zipCode)
        )

      extension (dto: AddressDto)
        def toUnvalidated: UnvalidatedAddress =
          UnvalidatedAddress(
            addressLine1 = dto.addressLine1,
            addressLine2 = dto.addressLine2,
            addressLine3 = dto.addressLine3,
            addressLine4 = dto.addressLine4,
            city = dto.city,
            country = dto.country,
            zipCode = dto.zipCode
          )
        def toDomain: Validation[ValidationError, Address] =
          for
            addressLine1 <- makeString50.requiredField("addressLine1", dto.addressLine1)
            addressLine2 <- makeString50.optionalField("addressLine2", dto.addressLine2)
            addressLine3 <- makeString50.optionalField("addressLine3", dto.addressLine2)
            addressLine4 <- makeString50.optionalField("addressLine4", dto.addressLine2)
            city         <- makeString50.requiredField("city", dto.city)
            country      <- makeCountry.requiredField("country", dto.country)
            zipCode      <- makeZipCode.requiredField("zipCode", dto.addressLine1)
          yield Address(
            addressLine1 = addressLine1,
            addressLine2 = addressLine2,
            addressLine3 = addressLine3,
            addressLine4 = addressLine4,
            city = city,
            country = country,
            zipCode = zipCode
          )
    end AddressDto

    case class OrderLineDto(orderLineId: String, productCode: String, quantity: Double)
    object OrderLineDto:
      extension (dto: OrderLineDto)
        def toUnvalidated: UnvalidatedOrderLine =
          UnvalidatedOrderLine(dto.orderLineId, dto.productCode, dto.quantity)

    case class OrderDto(
      orderId: String,
      customerInfo: CustomerInfoDto,
      shippingAddress: AddressDto,
      billingAddress: AddressDto,
      lines: Array[OrderLineDto],
      promotionCode: String
    )
    object OrderDto:
      extension (dto: OrderDto)
        def toUnvalidated: UnvalidatedOrder =

          import AddressDto.toUnvalidated
          import OrderLineDto.toUnvalidated
          import CustomerInfoDto.toUnvalidated

          UnvalidatedOrder(
            dto.orderId,
            dto.customerInfo.toUnvalidated,
            dto.shippingAddress.toUnvalidated,
            dto.billingAddress.toUnvalidated,
            if dto.lines == null then List.empty else dto.lines.map(_.toUnvalidated).toList,
            dto.promotionCode
          )

    case class ValidationErrorDto(fieldName: String, description: String)
    object ValidationErrorDto:

      def fromDomain(e: ValidationError): ValidationErrorDto =
        e match
          case Cause(description) => ValidationErrorDto("", description)
          case FieldError(fieldName, error) =>
            val nested              = fromDomain(error)
            val suffix              = if nested.fieldName == "" then "" else s".${nested.fieldName}"
            val normalizedFieldName = s"$fieldName$suffix"
            ValidationErrorDto(normalizedFieldName, nested.description)
          case IndexedFieldError(list, index, error) =>
            val nested              = fromDomain(error)
            val suffix              = if nested.fieldName == "" then "" else s".${nested.fieldName}"
            val normalizedFieldName = s"$list[$index]$suffix"
            ValidationErrorDto(normalizedFieldName, nested.description)

    end ValidationErrorDto

    import PlaceOrderErrorDto.Kind

    case class PlaceOrderErrorDto(
      kind: Kind,
      validationFailureErrors: List[ValidationErrorDto],
      pricingErrorCause: String
    )
    object PlaceOrderErrorDto:
      enum Kind:
        case ValidationFailure, PriceError

      def fromDomain(domain: PlaceOrderError): PlaceOrderErrorDto =
        domain match
          case PricingError(cause) => PlaceOrderErrorDto(kind = Kind.PriceError, null, cause)
          case vf @ ValidationFailure(errors) =>
            val errorDtos = errors.map(ValidationErrorDto.fromDomain)
            PlaceOrderErrorDto(kind = Kind.ValidationFailure, errorDtos.toList, "")
    end PlaceOrderErrorDto
  end Dto
