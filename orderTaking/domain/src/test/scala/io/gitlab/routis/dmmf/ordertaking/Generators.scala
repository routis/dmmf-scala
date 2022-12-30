package io.gitlab.routis.dmmf.ordertaking

import com.github.javafaker.Faker
import io.gitlab.routis.dmmf.ordertaking.domain.ZipCode
import zio.stream.ZStream
import zio.test.{ Gen, Sample }

object Generators:

  val zipCodeStringGen: Gen[Any, String] =
    val faker = new Faker()
    val zipCodes = List
      .fill(10)(faker.address().zipCode().take(5))
      .map(Option(_))
      .map(it => it.map(Sample.noShrink(_)))
    Gen(ZStream.fromIterable(zipCodes))
