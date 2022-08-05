package io.gitlab.routis.dmmf.ordertaking.pub.internal

import io.gitlab.routis.dmmf.ordertaking.pub.internal.Validations.ValidationError.{ indexFieldError, missingField }
import zio.{ IO, NonEmptyChunk, UIO }
import zio.prelude.{ Validation, ZValidation }
import io.gitlab.routis.dmmf.ordertaking.cmn.SmartConstructor
object Validations:

  import ValidationError.FieldName
  enum ValidationError:
    self =>

    case Cause(description: String)                                             extends ValidationError
    case FieldError(field: FieldName, error: ValidationError)                   extends ValidationError
    case IndexedFieldError(list: FieldName, index: Int, error: ValidationError) extends ValidationError

  object ValidationError:
    type FieldName = String

    val missing: ValidationError = Cause("Missing")

    private def causeOf(error: Any): ValidationError =
      error match
        case ve: ValidationError => ve
        case str: String         => Cause(str)
        case _                   => Cause(error.toString)
    def fieldError(field: FieldName, error: Any): ValidationError =
      FieldError(field, causeOf(error))
    def nestToField(field: FieldName): ValidationError => ValidationError = error => FieldError(field, error)

    private def missingField(field: FieldName): ValidationError = nestToField(field)(missing)

    def indexFieldError(listName: FieldName, index: Int): ValidationError => ValidationError =
      error => ValidationError.IndexedFieldError(listName, index, error)

    import SmartConstructor.{ changeError, optional, required }

    def ensurePresentOption[A](fieldName: FieldName, optionA: Option[A]): Validation[ValidationError, A] =
      Validation.fromEither(optionA.toRight(missingField(fieldName)))
    def ensurePresent[A](fieldName: FieldName, a: A): Validation[ValidationError, A] =
      ensurePresentOption(fieldName, Option(a))

    extension [A, E, B](smartConstructor: SmartConstructor[A, E, B])

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

  end ValidationError

  extension [E, A](x: IO[NonEmptyChunk[E], A])
    def uioValidation: UIO[Validation[E, A]] = x.either.map(Validation.fromEitherNonEmptyChunk)

  extension [E, A](x: UIO[Validation[E, A]]) def ioValidation: IO[NonEmptyChunk[E], A] = x.flatMap(v => v.toIO)

  extension [E, A](v: Validation[E, A])
    def toIO: zio.IO[NonEmptyChunk[E], A] =
      zio.ZIO.fromEither(v.toEither)
