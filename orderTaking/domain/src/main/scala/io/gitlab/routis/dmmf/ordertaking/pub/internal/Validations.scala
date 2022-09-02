package io.gitlab.routis.dmmf.ordertaking.pub.internal

import io.gitlab.routis.dmmf.ordertaking.cmn.ValidationError.{ indexFieldError, missingField }
import zio.{ IO, NonEmptyChunk, UIO }
import zio.prelude.{ Validation, ZValidation }
import io.gitlab.routis.dmmf.ordertaking.cmn.SmartConstructor
object Validations:

  extension [E, A](x: IO[NonEmptyChunk[E], A])
    def uioValidation: UIO[Validation[E, A]] = x.either.map(Validation.fromEitherNonEmptyChunk)

  extension [E, A](x: UIO[Validation[E, A]]) def ioValidation: IO[NonEmptyChunk[E], A] = x.flatMap(v => v.toIO)

  extension [E, A](v: Validation[E, A])
    def toIO: zio.IO[NonEmptyChunk[E], A] =
      zio.ZIO.fromEither(v.toEither)
