package io.gitlab.routis.dmmf.ordertaking.pub.internal

import zio.prelude.Validation
import zio.NonEmptyChunk

object Validations:

  type EitherV[E, A]             = Either[NonEmptyChunk[E], A]
  type FieldName                 = String
  type SmartConstructor[A, E, B] = A => Validation[E, B]

  import ValidationError.{ Cause, FieldError, IndexedFieldError }

  val missing: ValidationError = Cause("Missing")

  def fieldError(field: FieldName): ValidationError => ValidationError = error =>
    FieldError(field, error)

  def missingField(field: FieldName): ValidationError = fieldError(field)(missing)

  def nestErrors(
    field: FieldName
  ): NonEmptyChunk[ValidationError] => NonEmptyChunk[ValidationError] =
    _.map(fieldError(field))

  def indexFieldError(listName: FieldName, index: Int): ValidationError => ValidationError =
    error => ValidationError.IndexedFieldError(listName, index, error)

  private def causeOf(any: Any): ValidationError =
    any match
      case ve: ValidationError => ve
      case str: String         => Cause(str)
      case _                   => Cause(any.toString)

  enum ValidationError:
    self =>

    case Cause(description: String) extends ValidationError
    case FieldError(field: FieldName, error: ValidationError) extends ValidationError
    case IndexedFieldError(list: FieldName, index: Int, error: ValidationError)
        extends ValidationError

  private def requiredField[A, E, B](
    field: FieldName,
    fieldValue: A,
    validation: SmartConstructor[A, E, B]
  ): Validation[ValidationError, B] =
    fieldValue match
      case null => Validation.fail(missingField(field))
      case _    =>
        validation(fieldValue)
          .mapError(e => fieldError(field)(causeOf(e)))

  private def optionalField[A, E, B](
    field: FieldName,
    fieldValue: A,
    validation: SmartConstructor[A, E, B]
  ): Validation[ValidationError, Option[B]] =
    fieldValue match
      case null => Validation.succeed(Option.empty)
      case _    =>
        validation(fieldValue)
          .mapError(e => fieldError(field)(causeOf(e)))
          .map(Option(_))

  extension [E, A](v: Validation[E, A])
    def toZIOWithAllErrors: zio.IO[zio.NonEmptyChunk[E], A] =
      zio.ZIO.fromEither(v.toEither)

  extension [A, E, B](smartConstructor: SmartConstructor[A, E, B])
    def requiredField(field: FieldName, a: A): Validation[ValidationError, B]         =
      Validations.requiredField(field, a, smartConstructor)
    def optionalField(field: FieldName, a: A): Validation[ValidationError, Option[B]] =
      Validations.optionalField(field, a, smartConstructor)

  extension [E, A](either: EitherV[E, A])
    def toValidation: Validation[E, A] = Validation.fromEitherNonEmptyChunk(either)

  object SmartConstructorF:
    extension [F[_, _], A, E, B](smartConstructor: SmartConstructor[A, E, B])(using
      errorContainer: zio.prelude.Equivalence[F[ValidationError, B], Validation[ValidationError, B]]
    )
      def requiredField(field: FieldName, fieldValue: A): F[ValidationError, B] =
        errorContainer.from(Validations.requiredField(field, fieldValue, smartConstructor))

      def optionalField(field: FieldName, fieldValue: A): F[ValidationError, Option[B]] =
        Validations.optionalField(field, fieldValue, smartConstructor)
        ???
