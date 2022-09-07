package io.gitlab.routis.dmmf.ordertaking

import io.gitlab.routis.dmmf.ordertaking.domain.ValidationError.missingField
import zio.prelude.Validation

package object domain:
  //
  // New types (Value objects in DDD terms)
  //
  type OrderId       = OrderId.Type
  type OrderLineId   = OrderLineId.Type
  type Price         = BigDecimal
  type BillingAmount = zio.prelude.newtypes.Sum[BigDecimal]

  type ZipCode       = ZipCode.Type
  type EmailAddress  = EmailAddress.Type
  type PromotionCode = PromotionCode.Type

  //
  // Constructors for simple types
  //
  val makeOrderId: SmartConstructor[String, String, OrderId]         = OrderId.make
  val makeOrderLineId: SmartConstructor[String, String, OrderLineId] = OrderLineId.make
  val makeProductCode: SmartConstructor[String, String, ProductCode] = ProductCode.make
  def makeOrderQuantity(productCode: ProductCode): SmartConstructor[Double, String, OrderQuantity] =
    OrderQuantity.forProduct(productCode)

  val makeZipCode: SmartConstructor[String, String, ZipCode]             = ZipCode.make
  val makeEmailAddress: SmartConstructor[String, String, EmailAddress]   = EmailAddress.make
  val makeString50: SmartConstructor[String, String, String50]           = String50.make
  val makeVipStatus: SmartConstructor[String, String, VipStatus]         = VipStatus.make
  val makePromotionCode: SmartConstructor[String, String, PromotionCode] = PromotionCode.make
  //
  // Constructors for compound types (Value objects in DDD terms)
  //
  def makePersonalName[E](
    firstName: Validation[E, String50],
    lastName: Validation[E, String50]
  ): Validation[E, PersonalName] =
    Validation.validateWith(firstName, lastName)(PersonalName.apply)

  def makeAddress[E](
    addressLine1: Validation[E, String50],
    addressLine2: Validation[E, Option[String50]],
    addressLine3: Validation[E, Option[String50]],
    addressLine4: Validation[E, Option[String50]],
    city: Validation[E, String50],
    zipCode: Validation[E, ZipCode]
  ): Validation[E, Address] =
    Validation
      .validateWith(addressLine1, addressLine2, addressLine3, addressLine4, city, zipCode)(Address.apply)

  def makeCustomerInfo[E](
    name: Validation[E, PersonalName],
    emailAddress: Validation[E, EmailAddress],
    vipStatus: Validation[E, VipStatus]
  ): Validation[E, CustomerInfo] =
    Validation
      .validateWith(name, emailAddress, vipStatus)(CustomerInfo.apply)

  //
  // Validation Error
  //

  import ValidationError.FieldName

  enum ValidationError:
    self =>

    case Cause(description: String)                                             extends ValidationError
    case FieldError(field: FieldName, error: ValidationError)                   extends ValidationError
    case IndexedFieldError(list: FieldName, index: Int, error: ValidationError) extends ValidationError

  object ValidationError:
    type FieldName = String

    val missing: ValidationError = Cause("Missing")

    private[domain] def causeOf(error: Any): ValidationError =
      error match
        case ve: ValidationError => ve
        case str: String         => Cause(str)
        case _                   => Cause(error.toString)

    def fieldError(field: FieldName, error: Any): ValidationError =
      FieldError(field, causeOf(error))

    def nestToField(field: FieldName): ValidationError => ValidationError = error => FieldError(field, error)

    def missingField(field: FieldName): ValidationError = nestToField(field)(missing)

    def indexFieldError(listName: FieldName, index: Int): ValidationError => ValidationError =
      error => ValidationError.IndexedFieldError(listName, index, error)

    def ensurePresentOption[A](fieldName: FieldName, optionA: Option[A]): Validation[ValidationError, A] =
      Validation.fromEither(optionA.toRight(missingField(fieldName)))

    def ensurePresent[A](fieldName: FieldName, a: A): Validation[ValidationError, A] =
      ensurePresentOption(fieldName, Option(a))

  end ValidationError

  //
  // Smart Constructor
  //
  type SmartConstructor[A, E, B] = A => Validation[E, B]

  import zio.NonEmptyChunk
  import ValidationError.{ causeOf, fieldError, indexFieldError, missingField }

  extension [A, E, B](smartConstructor: SmartConstructor[A, E, B])

    def changeError[E1](f: E => E1): SmartConstructor[A, E1, B] =
      smartConstructor.andThen(_.mapError(f))

    def required(ifMissing: => E): SmartConstructor[Option[A], E, B] =
      optionA =>
        optionA match
          case Some(a) => smartConstructor(a)
          case None    => Validation.fail(ifMissing)

    def optional: SmartConstructor[Option[A], E, Option[B]] =
      optionA =>
        optionA match
          case Some(a) => smartConstructor(a).map(Some.apply)
          case None    => Validation.succeed(None)

    def nest(fieldName: FieldName): SmartConstructor[A, ValidationError, B] =
      smartConstructor.changeError(fieldError(fieldName, _))

    def requiredFieldFromOption(field: FieldName, oa: Option[A]): Validation[ValidationError, B] =
      smartConstructor
        .changeError(fieldError(field, _))
        .required(missingField(field))(oa)

    def requiredField(field: FieldName, a: A): Validation[ValidationError, B] =
      requiredFieldFromOption(field, Option(a))

    def optionalFieldFromOption(field: FieldName, optionA: Option[A]): Validation[ValidationError, Option[B]] =
      smartConstructor.changeError(fieldError(field, _)).optional(optionA)

    def optionalField(field: FieldName, a: A): Validation[ValidationError, Option[B]] =
      optionalFieldFromOption(field, Option(a))

    def nonEmptyChunkField(fieldName: FieldName, as: Iterable[A]): Validation[ValidationError, NonEmptyChunk[B]] =
      val notNullAs = Option(as).fold(Iterable.empty[A])(identity)
      val maybeNEC =
        NonEmptyChunk.fromIterableOption(notNullAs).toRight(missingField(fieldName))
      Validation
        .fromEither(maybeNEC)
        .flatMap(nec =>
          Validation.validateAll(nec.zipWithIndex.map { (a, index) =>
            smartConstructor.changeError(causeOf.andThen(indexFieldError(fieldName, index)))(a)
          })
        )
