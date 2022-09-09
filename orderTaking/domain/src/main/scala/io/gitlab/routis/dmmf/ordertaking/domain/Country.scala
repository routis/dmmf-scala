package io.gitlab.routis.dmmf.ordertaking.domain

import java.util.Locale
import zio.prelude.*
import Assertion.*

import scala.jdk.CollectionConverters.*

type Iso3166Part1Alpha2 = Iso3166.Iso3166Part1Alpha2.Type
type Iso3166Part1Alpha3 = Iso3166.Iso3166Part1Alpha3.Type

object Iso3166:

  object Iso3166Part1Alpha2 extends Subtype[String]:
    override inline def assertion: Assertion[String] =
      equalToOneOf(getISOCountries(Locale.IsoCountryCode.PART1_ALPHA2))

  object Iso3166Part1Alpha3 extends Subtype[String]:
    override inline def assertion: Assertion[String] =
      equalToOneOf(getISOCountries(Locale.IsoCountryCode.PART1_ALPHA3))

  private inline def equalToOneOf[A](inline as: Iterable[A]): Assertion[A] =
    given orIsIdentity[A]: Identity[Assertion[A]] with
      override def identity                                        = Assertion.never
      override def combine(l: => Assertion[A], r: => Assertion[A]) = l || r
    as.foldMap(equalTo)

  private def getISOCountries(isoType: Locale.IsoCountryCode): Iterable[String] =
    Locale.getISOCountries(isoType).asScala

  def main(args: Array[String]): Unit =
    val gr = Iso3166Part1Alpha2.makeAll(List("GR", "DE"))
    println(gr)
