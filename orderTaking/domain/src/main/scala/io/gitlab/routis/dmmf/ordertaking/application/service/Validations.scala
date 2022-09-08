package io.gitlab.routis.dmmf.ordertaking.application.service

import zio.prelude.Validation
import zio.{ IO, NonEmptyChunk, UIO, ZIO }

private[service] object Validations:

  extension [E, A](self: IO[NonEmptyChunk[E], A])
    def uioValidation: UIO[Validation[E, A]] = self.either.map(Validation.fromEitherNonEmptyChunk)

  extension [E, A](self: UIO[Validation[E, A]]) def ioValidation: IO[NonEmptyChunk[E], A] = self.flatMap(_.toIO)
  extension [E, A](self: Validation[E, A]) def toIO: zio.IO[NonEmptyChunk[E], A]          = ZIO.fromEither(self.toEither)
