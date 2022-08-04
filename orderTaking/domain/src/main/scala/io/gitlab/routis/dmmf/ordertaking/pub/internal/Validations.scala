package io.gitlab.routis.dmmf.ordertaking.pub.internal

import zio.NonEmptyChunk
import zio.prelude.Validation

object Validations:

  import ValidationError.FieldName
  enum ValidationError:
    self =>

    case Cause(description: String) extends ValidationError
    case FieldError(field: FieldName, error: ValidationError) extends ValidationError
    case IndexedFieldError(list: FieldName, index: Int, error: ValidationError)
        extends ValidationError

  object ValidationError:
    type FieldName = String
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

    import SmartConstructor.{ changeError, optional, required }
    extension [A, E, B](smartConstructor: SmartConstructor[A, E, B])

      def requiredFieldFromOption(field: FieldName, oa: Option[A]): Validation[ValidationError, B] =
        smartConstructor
          .changeError(e => fieldError(field)(causeOf(e)))
          .required(missingField(field))(oa)

      def requiredField(field: FieldName, a: A): Validation[ValidationError, B] =
        requiredFieldFromOption(field, Option(a))

      def optionalFieldFromOption(
        field: FieldName,
        optionA: Option[A]
      ): Validation[ValidationError, Option[B]] =
        smartConstructor.changeError(e => fieldError(field)(causeOf(e))).optional(optionA)

      def optionalField(field: FieldName, a: A): Validation[ValidationError, Option[B]] =
        optionalFieldFromOption(field, Option(a))
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

      def optional: SmartConstructor[Option[A], E, Option[B]] =
        optionA =>
          optionA match
            case Some(a) => smartConstructor(a).map(Some.apply)
            case None    => Validation.succeed(None)
  end SmartConstructor

  extension [E, A](v: Validation[E, A])
    def toZIOWithAllErrors: zio.IO[zio.NonEmptyChunk[E], A] =
      zio.ZIO.fromEither(v.toEither)

  extension [E, A](either: Either[NonEmptyChunk[E], A])
    def toValidation: Validation[E, A] = Validation.fromEitherNonEmptyChunk(either)
