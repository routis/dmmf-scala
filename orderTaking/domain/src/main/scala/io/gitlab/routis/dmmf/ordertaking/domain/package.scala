package io.gitlab.routis.dmmf.ordertaking

import org.joda.money.Money as JodaMoney
import zio.prelude.Assertion.*
import zio.prelude.{ Assertion, Newtype, Subtype, Validation }

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
  // Validation Error
  //

  type DomainValidation[A] = Validation[ValidationError, A]

  import ValidationError.{ causeOf, fieldError, indexFieldError, missingField }
  import zio.NonEmptyChunk

  enum ValidationError:
    case Cause(description: String)
    case FieldError(field: String, error: ValidationError)
    case IndexedFieldError(list: String, index: Int, error: ValidationError)

  object ValidationError:

    private val missing: ValidationError = Cause("Missing")

    private[domain] def causeOf(error: Any): ValidationError =
      error match
        case ve: ValidationError => ve
        case str: String         => Cause(str)
        case _                   => Cause(error.toString)

    def fieldError[E](field: String): E => ValidationError = error => fieldError(field, error)
    def fieldError(field: String, error: Any): ValidationError =
      FieldError(field, causeOf(error))

    def missingField(field: String): ValidationError = FieldError(field, missing)

    def indexFieldError(listName: String, index: Int): ValidationError => ValidationError =
      error => ValidationError.IndexedFieldError(listName, index, error)

    def ensurePresentOption[A](fieldName: String, optionA: Option[A]): Validation[ValidationError, A] =
      Validation.fromEither(optionA.toRight(missingField(fieldName)))

    def ensurePresent[A](fieldName: String, a: A): Validation[ValidationError, A] =
      ensurePresentOption(fieldName, Option(a))

  end ValidationError

  type SmartConstructor[A, E, B] = A => Validation[E, B]
  extension [A, E, B](self: SmartConstructor[A, E, B])

    def nest(fieldName: String): A => DomainValidation[B] =
      self.andThen(_.mapError(fieldError(fieldName)))

    def requiredField(fieldName: String, a: A): DomainValidation[B] =
      Option(a) match
        case Some(a) => self(a).mapError(fieldError(fieldName))
        case None    => Validation.fail(missingField(fieldName))

    def optionalField(fieldName: String, a: A): DomainValidation[Option[B]] =
      Option(a) match
        case Some(a) => self(a).mapError(fieldError(fieldName)).map(Some.apply)
        case None    => Validation.succeed(None)

    def chunkField(fieldName: String, as: Iterable[A]): DomainValidation[zio.Chunk[B]] =
      val notNullAs = Option(as).fold(Iterable.empty[A])(identity)
      val chunkAs   = zio.Chunk.fromIterable(notNullAs)
      val validations = chunkAs.zipWithIndex.map { (a, index) =>
        self(a).mapError(causeOf.andThen(indexFieldError(fieldName, index)))
      }
      Validation.validateAll(validations)
    def nonEmptyChunkField(fieldName: String, as: Iterable[A]): DomainValidation[NonEmptyChunk[B]] =
      chunkField(fieldName, as).flatMap(validAs =>
        val maybeNec = NonEmptyChunk.fromChunk(validAs).toRight(missingField(fieldName))
        Validation.fromEither(maybeNec)
      )
