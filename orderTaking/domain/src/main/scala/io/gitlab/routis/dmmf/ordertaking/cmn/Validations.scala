package io.gitlab.routis.dmmf.ordertaking.cmn

import zio.prelude.{ Validation, ZValidation }
import zio.{ NonEmptyChunk, ZIO }

object Validations:

  type SimpleValidation[A]    = Validation[String, A]
  type FieldName              = String
  type SmartConstructor[A, B] = A => SimpleValidation[B]

  def missingField(field: FieldName): ValidationError =
    ValidationError.FieldError(field = field, description = "Missing")

  def fieldError(field: FieldName): String => ValidationError               = e =>
    ValidationError.FieldError(field = field, description = e)
  def nestError(parentField: FieldName): ValidationError => ValidationError = error =>
    error match
      case ValidationError.FieldError(field, description) =>
        ValidationError.FieldError(parentField + "." + field, description)
      case _                                              => error

  enum ValidationError:
    case FieldError(field: FieldName, description: String) extends ValidationError

  def requiredField[A, B](
    field: FieldName,
    fieldValue: A,
    validation: SmartConstructor[A, B]
  ): Validation[ValidationError, B] =
    fieldValue match
      case null => Validation.fail(missingField(field))
      case _    =>
        validation(fieldValue)
          .mapError(fieldError(field))

  def optionalField[A, B](
    field: FieldName,
    fieldValue: A,
    validation: SmartConstructor[A, B]
  ): Validation[ValidationError, Option[B]] =
    fieldValue match
      case null => Validation.succeed(Option.empty)
      case _    =>
        validation(fieldValue)
          .mapError(fieldError(field))
          .map(Option(_))

  extension [E, A](v: Validation[E, A])
    def toZIOWithAllErrors: zio.IO[zio.NonEmptyChunk[E], A] =
      zio.ZIO.fromEither(v.toEither)

  extension [A, B](smartConstructor: SmartConstructor[A, B])
    def requiredField(field: FieldName, a: A): Validation[ValidationError, B]         =
      Validations.requiredField(field, a, smartConstructor)
    def optionalField(field: FieldName, a: A): Validation[ValidationError, Option[B]] =
      Validations.optionalField(field, a, smartConstructor)

  extension [R, E, A](zio: ZIO[R, NonEmptyChunk[E], A])
    def toValidation: ZIO[R, Nothing, Validation[E, A]] =
      zio.either.map(Validation.fromEitherNonEmptyChunk)
