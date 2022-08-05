package io.gitlab.routis.dmmf.ordertaking.pub.internal

import io.gitlab.routis.dmmf.ordertaking.pub.internal.Validations.ValidationError.{ indexFieldError, missingField }
import zio.NonEmptyChunk
import zio.prelude.{ Validation, ZValidation }

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

  type SmartConstructor[A, E, B] = A => Validation[E, B]
  object SmartConstructor:

    extension [A, E, B](smartConstructor: SmartConstructor[A, E, B])

      def changeError[E1](f: E => E1): SmartConstructor[A, E1, B] =
        smartConstructor.andThen(_.mapError(f))

      def required(ifMissing: => E): SmartConstructor[Option[A], E, B] =
        optionA =>
          optionA match
            case Some(a) => smartConstructor(a)
            case None    => Validation.fail(ifMissing)
      def requiredNullable(ifMissing: => E): SmartConstructor[A, E, B] =
        a => required(ifMissing)(Option(a))
      def optional: SmartConstructor[Option[A], E, Option[B]] =
        optionA =>
          optionA match
            case Some(a) => smartConstructor(a).map(Some.apply)
            case None    => Validation.succeed(None)

  end SmartConstructor

  type AsyncValidation[E, A] = zio.IO[NonEmptyChunk[E], A]

  extension [E, A](v: Validation[E, A])
    def toAsync: AsyncValidation[E, A] =
      zio.ZIO.fromEither(v.toEither)

  extension [E, A](either: Either[NonEmptyChunk[E], A])
    def toValidation: Validation[E, A] = Validation.fromEitherNonEmptyChunk(either)
