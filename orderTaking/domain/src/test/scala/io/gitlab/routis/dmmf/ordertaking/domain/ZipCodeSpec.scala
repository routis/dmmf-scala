package io.gitlab.routis.dmmf.ordertaking.domain

import io.gitlab.routis.dmmf.ordertaking.Generators
import io.gitlab.routis.dmmf.ordertaking.domain.ZipCode
import zio.test.*

object ZipCodeSpec extends ZIOSpecDefault:

  override def spec: Spec[Any, Nothing] = suite("ZipCode")(
    test("Check make with valid") {
    check(Generators.zipCodeStringGen)(str => assertTrue(ZipCode.make(str).toOption.isDefined))
  },
    test("Check make with invalid") {
      check(Gen.string)(str=>assertTrue(ZipCode.make(str).toOption.isEmpty))
    }
  )
