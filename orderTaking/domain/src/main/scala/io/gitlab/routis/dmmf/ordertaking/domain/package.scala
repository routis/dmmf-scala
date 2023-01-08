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
  type Country       = Iso3166.Part1Alpha2.Type
  type EmailAddress  = EmailAddress.Type
  type PromotionCode = PromotionCode.Type
  type String50      = String50.Type

  //
  // Constraint definitions
  //
  object OrderId     extends Newtype[String]
  object OrderLineId extends Newtype[String]
  object ZipCode extends Newtype[String]:
    override inline def assertion: Assertion[String] = matches("^\\d{5}$".r)
  object EmailAddress extends Newtype[String]:
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
  val makeCountry: SmartConstructor[String, String, Country]             = Iso3166.Part1Alpha2.make
  val makeEmailAddress: SmartConstructor[String, String, EmailAddress]   = EmailAddress.make
  val makeString50: SmartConstructor[String, String, String50]           = String50.make
  val makeVipStatus: SmartConstructor[String, String, VipStatus]         = VipStatus.make
  val makePromotionCode: SmartConstructor[String, String, PromotionCode] = PromotionCode.make

  //
  // Smart Constructor
  //
  type SmartConstructor[A, E, B] = A => Validation[E, B]

  import zio.NonEmptyChunk
  import ValidationError.{ causeOf, fieldError, indexFieldError, missingField }

  object SmartConstructor:

    def changeError[A, E, E1, B](sc: SmartConstructor[A, E, B])(f: E => E1): SmartConstructor[A, E1, B] =
      sc.andThen(_.mapError(f))

    def required[A, E, B](sc: SmartConstructor[A, E, B])(ifMissing: => E): SmartConstructor[Option[A], E, B] =
      optionA =>
        optionA match
          case Some(a) => sc(a)
          case None    => Validation.fail(ifMissing)

    def optional[A, E, B](sc: SmartConstructor[A, E, B]): SmartConstructor[Option[A], E, Option[B]] =
      optionA =>
        optionA match
          case Some(a) => sc(a).map(Some.apply)
          case None    => Validation.succeed(None)
  end SmartConstructor

  //
  // Validation Error
  //

  import ValidationError.FieldName

  enum ValidationError:
    case Cause(description: String)
    case FieldError(field: FieldName, error: ValidationError)
    case IndexedFieldError(list: FieldName, index: Int, error: ValidationError)

  object ValidationError:
    type FieldName = String

    private val missing: ValidationError = Cause("Missing")

    private[domain] def causeOf(error: Any): ValidationError =
      error match
        case ve: ValidationError => ve
        case str: String         => Cause(str)
        case _                   => Cause(error.toString)

    def fieldError[E](field: FieldName): E => ValidationError = error => fieldError(field, error)
    def fieldError(field: FieldName, error: Any): ValidationError =
      FieldError(field, causeOf(error))

    def missingField(field: FieldName): ValidationError = FieldError(field, missing)

    def indexFieldError(listName: FieldName, index: Int): ValidationError => ValidationError =
      error => ValidationError.IndexedFieldError(listName, index, error)

    def ensurePresentOption[A](fieldName: FieldName, optionA: Option[A]): Validation[ValidationError, A] =
      Validation.fromEither(optionA.toRight(missingField(fieldName)))

    def ensurePresent[A](fieldName: FieldName, a: A): Validation[ValidationError, A] =
      ensurePresentOption(fieldName, Option(a))

  end ValidationError

  extension [A, E, B](self: SmartConstructor[A, E, B])

    def nest(fieldName: FieldName): SmartConstructor[A, ValidationError, B] =
      SmartConstructor.changeError(self)(fieldError(fieldName))

    def requiredFieldFromOption(fieldName: FieldName, oa: Option[A]): Validation[ValidationError, B] =
      SmartConstructor.required(self.nest(fieldName))(missingField(fieldName))(oa)

    def requiredField(fieldName: FieldName, a: A): Validation[ValidationError, B] =
      requiredFieldFromOption(fieldName, Option(a))

    def optionalFieldFromOption(fieldName: FieldName, optionA: Option[A]): Validation[ValidationError, Option[B]] =
      SmartConstructor.optional(nest(fieldName))(optionA)

    def optionalField(field: FieldName, a: A): Validation[ValidationError, Option[B]] =
      optionalFieldFromOption(field, Option(a))

    def chunkField(fieldName: FieldName, as: Iterable[A]): Validation[ValidationError, zio.Chunk[B]] =
      val notNullAs = Option(as).fold(Iterable.empty[A])(identity)
      val chunkAs   = zio.Chunk.fromIterable(notNullAs)
      val validations = chunkAs.zipWithIndex.map { (a, index) =>
        SmartConstructor.changeError(self)(causeOf.andThen(indexFieldError(fieldName, index)))(a)
      }
      Validation.validateAll(validations)
    def nonEmptyChunkField(fieldName: FieldName, as: Iterable[A]): Validation[ValidationError, NonEmptyChunk[B]] =
      chunkField(fieldName, as).flatMap(validAs =>
        val maybeNec = NonEmptyChunk.fromChunk(validAs).toRight(missingField(fieldName))
        Validation.fromEither(maybeNec)
      )
