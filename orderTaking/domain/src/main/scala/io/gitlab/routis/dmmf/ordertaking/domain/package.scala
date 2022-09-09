package io.gitlab.routis.dmmf.ordertaking

import io.gitlab.routis.dmmf.ordertaking.domain.ValidationError.missingField
import org.joda.money.Money as JodaMoney
import zio.prelude.{ Assertion, Newtype, Subtype, Validation }
import zio.prelude.Assertion.*

package object domain:

  //
  // New types (Value objects in DDD terms)
  //
  type OrderId       = OrderId.Type
  type OrderLineId   = OrderLineId.Type
  type Price         = Price.Type
  type BillingAmount = BillingAmount.Type
  type ZipCode       = ZipCode.Type
  type EmailAddress  = EmailAddress.Type
  type PromotionCode = PromotionCode.Type
  type String50      = String50.Type

  //
  // Constraint definitions
  //
  object OrderId     extends Newtype[String]
  object OrderLineId extends Newtype[String]
  object ZipCode extends Subtype[String]:
    override inline def assertion: Assertion[String] = matches("^\\d{5}$".r)
  object EmailAddress extends Subtype[String]:
    override inline def assertion: Assertion[String] = matches("^(.+)@(.+)$".r)
  object String50 extends Subtype[String]:
    override inline def assertion: Assertion[String] = hasLength(between(1, 50))
  object PromotionCode extends Newtype[String]

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

  extension [A, E, B](self: SmartConstructor[A, E, B])

    def changeError[E1](f: E => E1): SmartConstructor[A, E1, B] =
      self.andThen(_.mapError(f))

    def required(ifMissing: => E): SmartConstructor[Option[A], E, B] =
      optionA =>
        optionA match
          case Some(a) => self(a)
          case None    => Validation.fail(ifMissing)

    def optional: SmartConstructor[Option[A], E, Option[B]] =
      optionA =>
        optionA match
          case Some(a) => self(a).map(Some.apply)
          case None    => Validation.succeed(None)

    def nest(fieldName: FieldName): SmartConstructor[A, ValidationError, B] =
      self.changeError(fieldError(fieldName, _))

    def requiredFieldFromOption(field: FieldName, oa: Option[A]): Validation[ValidationError, B] =
      self
        .changeError(fieldError(field, _))
        .required(missingField(field))(oa)

    def requiredField(field: FieldName, a: A): Validation[ValidationError, B] =
      requiredFieldFromOption(field, Option(a))

    def optionalFieldFromOption(field: FieldName, optionA: Option[A]): Validation[ValidationError, Option[B]] =
      self.changeError(fieldError(field, _)).optional(optionA)

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
            self.changeError(causeOf.andThen(indexFieldError(fieldName, index)))(a)
          })
        )
