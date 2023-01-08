package io.gitlab.routis.dmmf.ordertaking.domain

import com.github.javafaker.Faker
import io.gitlab.routis.dmmf.ordertaking.Generators
import io.gitlab.routis.dmmf.ordertaking.domain.ZipCode
import zio.test.*
import zio.{ Random, ZIO, ZLayer }

object ZipCodeSpec extends ZIOSpecDefault:

  override def spec: Spec[Any, Nothing] = suite("ZipCode")(
    test("make() with valid input") {
      check(Generators.zipCodeStringGen) { str =>
        assert(ZipCode.make(str).toEither)(Assertion.equalTo(Right(str)))
      }
    },
    test("make() with invalid") {
      check(Gen.string)(str => assertTrue(ZipCode.make(str).toOption.isEmpty))
    },
    test("foo") {
      check(Generators.unvalidatedOrderGen) { orderLine =>
        println(orderLine)
        assertTrue(true)
      }
    }
  ).provideLayerShared(Generators.faker)
