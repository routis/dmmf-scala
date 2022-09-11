package io.gitlab.routis.dmmf.ordertaking.domain

import java.util.Locale
import zio.prelude.*
import Assertion.*

object Iso3166:

  object Part1Alpha2 extends Subtype[String]:
    override inline def assertion: Assertion[String] =
      hasLength(equalTo(2)) && equalToOneOf(getISOCountries(Locale.IsoCountryCode.PART1_ALPHA2))

  object Part1Alpha3 extends Subtype[String]:
    override inline def assertion: Assertion[String] =
      hasLength(equalTo(3)) && equalToOneOf(getISOCountries(Locale.IsoCountryCode.PART1_ALPHA3))

  private def equalToOneOf[A](as: Iterable[A]): Assertion[A] =
    given orIsIdentity[A]: Identity[Assertion[A]] with
      override def identity: Assertion[A]                          = Assertion.never
      override def combine(l: => Assertion[A], r: => Assertion[A]) = l || r
    as.foldMap(equalTo)

  private def getISOCountries(isoType: Locale.IsoCountryCode): Iterable[String] =
    import scala.jdk.CollectionConverters.*
    Locale.getISOCountries(isoType).asScala

  def main(args: Array[String]): Unit =
    val gr = Part1Alpha2.makeAll(List("GR", "DE"))
    println(gr)
