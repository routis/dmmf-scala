package io.gitlab.routis.dmmf.ordertaking.domain

import zio.NonEmptyChunk
import zio.prelude.Validation
import ValidationError.{ causeOf, fieldError, indexFieldError, missingField }
import zio.NonEmptyChunk
import zio.prelude.Validation

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

end ValidationError

type DomainValidation[A] = Validation[ValidationError, A]

def ensurePresent[A](fieldName: String): A => DomainValidation[A] =
  a => Validation.fromEither(Option(a).toRight(missingField(fieldName)))

extension [A, E, B](smartConstructor: A => Validation[E, B])

  def nest(fieldName: String): A => DomainValidation[B] =
    smartConstructor.andThen(_.mapError(fieldError(fieldName)))

  def requiredField(fieldName: String): A => DomainValidation[B] =
    a =>
      Option(a) match
        case Some(a) => smartConstructor(a).mapError(fieldError(fieldName))
        case None    => Validation.fail(missingField(fieldName))

  def optionalField(fieldName: String): A => DomainValidation[Option[B]] =
    a =>
      Option(a) match
        case Some(a) => smartConstructor(a).mapError(fieldError(fieldName)).map(Some.apply)
        case None    => Validation.succeed(None)

  def chunkField(fieldName: String, as: Iterable[A]): DomainValidation[zio.Chunk[B]] =
    val notNullAs = Option(as).fold(Iterable.empty[A])(identity)
    val chunkAs   = zio.Chunk.fromIterable(notNullAs)
    val validations = chunkAs.zipWithIndex.map { (a, index) =>
      smartConstructor(a).mapError(causeOf.andThen(indexFieldError(fieldName, index)))
    }
    Validation.validateAll(validations)
  def nonEmptyChunkField(fieldName: String): Iterable[A] => DomainValidation[NonEmptyChunk[B]] =
    as =>
      chunkField(fieldName, as).flatMap(validAs =>
        val maybeNec = NonEmptyChunk.fromChunk(validAs).toRight(missingField(fieldName))
        Validation.fromEither(maybeNec)
      )
